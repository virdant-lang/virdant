use std::sync::Arc;

use bstr::{BString, ByteSlice};
use hashbrown::HashMap;

use crate::analysis::Location;
use crate::analysis::symbols::SymbolKind;
use crate::common::{TypeScheme, Width};
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::types::{Type, TypeDef};
use crate::db::Builder;

pub(crate) fn build_typedefs(builder: &mut Builder) -> Vec<TypeDef> {
    let mut typedefs = vec![];
    let symboltable = builder.get_symboltable();
    for item_symbol in symboltable.typedefs() {
        let kind = match item_symbol.kind() {
            SymbolKind::UnionDef => TypeScheme::UnionDef,
            SymbolKind::StructDef => TypeScheme::StructDef,
            SymbolKind::EnumDef => TypeScheme::EnumDef,
            SymbolKind::BuiltinDef => TypeScheme::BuiltinDef,
            _ => unreachable!(),
        };
        typedefs.push(TypeDef {
            fqn: item_symbol.fqn().into(),
            location: item_symbol.location(),
            kind,
        });
    }
    typedefs
}

#[derive(Debug)]
pub struct TypeIndex {
    typs: Vec<Type>,
    typ_at_location: HashMap<Location, usize>, // index into typs
    diagnostics: Vec<Diagnostic>,
}

impl TypeIndex {
    pub fn typs(&self) -> &Vec<Type> {
        &self.typs
    }

    pub fn type_at(&self, location: Location) -> Option<&Type> {
        self.typ_at_location
            .get(&location)
            .map(|type_id| &self.typs[*type_id])
    }
}

pub(crate) fn build_type_index(builder: &mut Builder) -> Arc<TypeIndex> {
    let mut type_index = TypeIndex {
        typs: vec![],
        typ_at_location: HashMap::new(),
        diagnostics: vec![],
    };

    for package in builder.get_packages() {
        let parsing = builder.get_parsing(package);
        type_index.gather_type_roots(builder, parsing.root());
    }

    Arc::new(type_index)
}


impl TypeIndex {
    fn gather_type_roots(&mut self, builder: &mut Builder, node: AstNode<'_>) {
        if let AstNodePayload::Type(_) = node.payload() {
            let parsing = node.parsing;
            let type_name = match node.child(0).payload() {
                AstNodePayload::Ofness(ofness) => {
                    let _package = ofness
                        .package
                        .map(|pkg| parsing.string(pkg).to_owned())
                        .unwrap_or_else(|| BString::from(b"builtin".to_vec()));
                    parsing.string(ofness.name)
                }
                _ => panic!("expected Ofness inside Type node"),
            };

            let symboltable = builder.get_symboltable();
            if let Some(symbol) = symboltable.resolve_item(type_name, parsing.package()) {
                let bit_symbol = symboltable.resolve(b"builtin::Bit".into()).unwrap();
                let word_symbol = symboltable.resolve(b"builtin::Word".into()).unwrap();
                let clock_symbol = symboltable.resolve(b"builtin::Clock".into()).unwrap();

                let typ = if symbol.id() == bit_symbol.id() {
                    Type::Bit
                } else if symbol.id() == clock_symbol.id() {
                    Type::Clock
                } else if symbol.id() == word_symbol.id() {
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

                let idx = self.typs.len();
                self.typs.push(typ);

                self.typ_at_location.insert(node.location(), idx);
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
            self.gather_type_roots(builder, child);
        }
    }
}
