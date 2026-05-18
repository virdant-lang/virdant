use indexmap::{IndexMap, IndexSet};

use crate::analysis::location::Location;
use crate::analysis::symbols::{SymbolId, SymbolTable};
use crate::common::{TypeScheme, Width, WordValue};

use std::sync::Arc;

use bstr::ByteSlice;
use bstr::BString;

use crate::analysis::symbols::SymbolKind;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;
use crate::db::Builder;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDef {
    pub symbol_id: SymbolId,
    pub kind: TypeScheme,

    pub width: Option<Width>,
    pub enumerant_values: IndexMap<SymbolId, WordValue>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct TypeId(usize);

#[derive(Debug)]
pub struct TypeIndex {
    typs: IndexSet<Type>,
    typ_at_location: IndexMap<Location, TypeId>,
    diagnostics: Vec<Diagnostic>,
}

pub(crate) fn build_typedefs(builder: &mut Builder) -> Arc<Vec<TypeDef>> {
    let mut typedefs = vec![];
    let symboltable = builder.get_symboltable();
    let item_symbols: Vec<_> = symboltable.typedefs();

    for item_symbol in item_symbols {
        let kind = match item_symbol.kind() {
            SymbolKind::UnionDef => TypeScheme::UnionDef,
            SymbolKind::StructDef => TypeScheme::StructDef,
            SymbolKind::EnumDef => TypeScheme::EnumDef,
            SymbolKind::BuiltinDef => TypeScheme::BuiltinDef,
            _ => unreachable!(),
        };

        let (width, enumerant_values) = if kind == TypeScheme::EnumDef {
            let location = item_symbol.location();
            let parsing = builder.get_parsing(location.package());
            let symboltable = builder.get_symboltable();
            let enumdef_node = parsing.ast_node(location.ast_node_id());
            let AstNodePayload::EnumDef(enum_def) = enumdef_node.payload() else { unreachable!() };

            let mut enumerant_values = IndexMap::new();
            for enumerant_node in enumdef_node.children() {
                let AstNodePayload::Enumerant(enumerant) = enumerant_node.payload() else { continue; };
                let enumerant_name = parsing.string(enumerant.name);
                let enumerant_id = {
                    let Some(enumerant_symbol) = symboltable.slot(item_symbol.id(), enumerant_name) else { continue; };
                    enumerant_symbol.id()
                };
                let expr_node = enumerant_node.child(0);
                let value = eval_const_expr(&expr_node);
                enumerant_values.insert(enumerant_id, value);
            }

            (Some(enum_def.width), enumerant_values)
        } else {
            (None, IndexMap::new())
        };

        typedefs.push(TypeDef {
            symbol_id: item_symbol.id(),
            kind,
            width,
            enumerant_values,
        });
    }
    Arc::new(typedefs)
}

pub(crate) fn build_typedef(builder: &mut Builder, symbol_id: SymbolId) -> Arc<TypeDef> {
    let symboltable = builder.get_symboltable();
    let item_symbol = symboltable.symbol(symbol_id);

    let kind = match item_symbol.kind() {
        SymbolKind::UnionDef => TypeScheme::UnionDef,
        SymbolKind::StructDef => TypeScheme::StructDef,
        SymbolKind::EnumDef => TypeScheme::EnumDef,
        SymbolKind::BuiltinDef => TypeScheme::BuiltinDef,
        _ => unreachable!(),
    };

    let (width, enumerant_values) = if kind == TypeScheme::EnumDef {
        let location = item_symbol.location();
        let parsing = builder.get_parsing(location.package());
        let symboltable = builder.get_symboltable();
        let enumdef_node = parsing.ast_node(location.ast_node_id());
        let AstNodePayload::EnumDef(enum_def) = enumdef_node.payload() else { unreachable!() };

        let mut enumerant_values = IndexMap::new();
        for enumerant_node in enumdef_node.children() {
            let AstNodePayload::Enumerant(enumerant) = enumerant_node.payload() else { continue; };
            let enumerant_name = parsing.string(enumerant.name);
            let enumerant_id = {
                let Some(enumerant_symbol) = symboltable.slot(item_symbol.id(), enumerant_name) else { continue; };
                enumerant_symbol.id()
            };
            let expr_node = enumerant_node.child(0);
            let value = eval_const_expr(&expr_node);
            enumerant_values.insert(enumerant_id, value);
        }

        (Some(enum_def.width), enumerant_values)
    } else {
        (None, IndexMap::new())
    };

    Arc::new(TypeDef {
        symbol_id: item_symbol.id(),
        kind,
        width,
        enumerant_values,
    })
}

fn eval_const_expr(node: &AstNode<'_>) -> WordValue {
    let parsing = node.parsing;
    match node.payload() {
        AstNodePayload::ExprWordLit(expr_word_lit) => {
            let literal = parsing.string(expr_word_lit.literal).to_str_lossy().into_owned();
            let (value, _width) = parse_word_literal(&literal);
            value
        }
        AstNodePayload::ExprParen => eval_const_expr(&node.child(0)),
        _ => panic!("Unsupported expression in enumerant value: {:?}", node.summary()),
    }
}

fn parse_word_literal(literal: &str) -> (WordValue, Option<Width>) {
    if let Some((value, width)) = literal.split_once('w') {
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

fn parse_nat_literal(literal: &str) -> WordValue {
    let literal = literal.replace('_', "");
    if let Some(hex) = literal.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap()
    } else if let Some(bin) = literal.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap()
    } else {
        literal.parse().unwrap()
    }
}

impl TypeIndex {
    pub fn typs(&self) -> &IndexSet<Type> {
        &self.typs
    }

    pub fn type_at(&self, location: Location) -> Option<&Type> {
        self.typ_at_location
            .get(&location)
            .map(|type_id| &self.typs[type_id.0])
    }

    pub fn type_id_at(&self, location: Location) -> Option<TypeId> {
        self.typ_at_location
            .get(&location)
            .copied()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    fn typ_to_index(&self, typ: &Type) -> Option<usize> {
        for (idx, typ_) in self.typs.iter().enumerate() {
            if typ == typ_ {
                return Some(idx);
            }
        }
        None
    }

    fn gather_type_roots(&mut self, builder: &mut Builder, node: AstNode<'_>, symboltable: &SymbolTable) {
        if let AstNodePayload::Type(_) = node.payload() {
            let parsing = node.parsing;
            let type_name: BString = match node.child(0).payload() {
                AstNodePayload::Ofness(ofness) => {
                    if let Some(package) = ofness.package {
                        let package_name = parsing.string(package);
                        let typ_name = parsing.string(ofness.name);
                        format!("{}::{}", package_name, typ_name).into()
                    } else {
                        parsing.string(ofness.name).to_owned()
                    }
                }
                _ => panic!("expected Ofness inside Type node"),
            };

            if let Some(symbol) = symboltable.resolve_item(type_name.as_bstr(), parsing.package()) {
                let bit_symbol = symboltable.resolve(b"builtin::Bit".into()).unwrap();
                let word_symbol = symboltable.resolve(b"builtin::Word".into()).unwrap();
                let clock_symbol = symboltable.resolve(b"builtin::Clock".into()).unwrap();
                let reset_symbol = symboltable.resolve(b"builtin::Reset".into()).unwrap();

                let typ = if symbol.id() == bit_symbol.id() {
                    if node.children().len() >= 2 {
                        self.diagnostics.push(diagnostics::Unknown {
                            region: node.region(),
                            message: "Type Bit takes no parameters.".into(),
                        }.into());
                        return;
                    }
                    Type::Bit
                } else if symbol.id() == clock_symbol.id() {
                    if node.children().len() >= 2 {
                        self.diagnostics.push(diagnostics::Unknown {
                            region: node.region(),
                            message: "Type Clock takes no parameters.".into(),
                        }.into());
                        return;
                    }
                    Type::Clock
                } else if symbol.id() == reset_symbol.id() {
                    if node.children().len() >= 2 {
                        self.diagnostics.push(diagnostics::Unknown {
                            region: node.region(),
                            message: "Type Reset takes no parameters.".into(),
                        }.into());
                        return;
                    }
                    Type::Reset
                } else if symbol.id() == word_symbol.id() {
                    if node.children().len() < 2 {
                        self.diagnostics.push(diagnostics::Unknown {
                            region: node.region(),
                            message: "Word type requires a width parameter.".into(),
                        }.into());
                        return;
                    }
                    let generics_node = node.child(1);
                    let AstNodePayload::GenericsParams(generics_params) = generics_node.payload() else {
                        unreachable!()
                    };
                    let spelling = parsing.string(generics_params.value).to_str_lossy().into_owned();
                    let width = spelling.parse::<Width>().unwrap();
                    Type::Word(width)
                } else {
                    Type::Usual(symbol.id())
                };

                let idx = if let Some(idx) = self.typ_to_index(&typ) {
                    idx
                } else {
                    self.typs.len()
                };

                self.typs.insert(typ);

                self.typ_at_location.insert(node.location(), TypeId(idx));
            } else {
                self.diagnostics.push(diagnostics::UnresolvedType {
                    region: node.region(),
                    typ: type_name.into(),
                }.into());
            }

            // Don't recurse into the children of a Type node (Ofness, GenericsParams, etc.)
            return;
        }

        for child in node.children() {
            self.gather_type_roots(builder, child, symboltable);
        }
    }
}

pub(crate) fn build_type_index(builder: &mut Builder) -> Arc<TypeIndex> {
    let mut type_index = TypeIndex {
        typs: IndexSet::new(),
        typ_at_location: IndexMap::new(),
        diagnostics: vec![],
    };

    let symboltable = builder.get_symboltable();

    let packages = builder.get_packages();
    for package in packages.iter() {
        let parsing = builder.get_parsing(package.clone());
        type_index.gather_type_roots(builder, parsing.root(), &symboltable);
    }

    Arc::new(type_index)
}
