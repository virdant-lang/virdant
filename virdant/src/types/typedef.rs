//! Defines `TypeDef`, `TypeId`, and `TypeIndex` for user-defined types
//! (unions, structs, enums, builtins) and mapping AST locations to
//! types; `build_typedefs` populates the global list including
//! evaluating constant expressions for enumerant values.
//!
//! Generic type definitions carry their declared parameters
//! (`[T : Type]`, `[n : Nat]`) on their `TypeDef`; their member types
//! are stored *as written*, containing `Type::Param` references to the
//! declared parameters.  Substitution to a mono type happens only at
//! use sites, via `types::substitute::substitute`.

use indexmap::{IndexMap, IndexSet};

use crate::analysis::location::Location;
use crate::analysis::symbols::{SymbolId, SymbolTable, Symbol, SymbolKind};
use crate::common::{TypeScheme, Width, WordValue};
use crate::types::typ::{Kind, ParamId};

use std::sync::Arc;

use bstr::ByteSlice;
use bstr::{BStr, BString};

use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;
use crate::db::Builder;
use crate::util::parse_word_literal;

/// One declared generic parameter of a type definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam {
    pub name: BString,
    pub kind: Kind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDef {
    pub symbol_id: SymbolId,
    pub kind: TypeScheme,

    /// The generic parameters declared by the def, in declaration
    /// order.  `ParamId`s in member types index into this list.
    pub params: Vec<TypeParam>,

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

/// The generic parameter declaration list (`Generics` AST node) of an
/// item, if it declares one.
pub fn generics_decl_of<'p>(node: &'p AstNode<'p>) -> Option<AstNode<'p>> {
    node.children()
        .into_iter()
        .find(|child| matches!(child.payload(), AstNodePayload::Generics))
}

/// Parse a `Generics` declaration node into its parameter list.
///
/// Unknown kind names map to `Kind::Type`; the declaration-site
/// diagnostic for those lives in the `Check` pass
/// (`check_generic_param_kinds`).
pub(crate) fn parse_generics_decl(node: &AstNode<'_>) -> Vec<TypeParam> {
    let parsing = node.parsing();
    let mut params = vec![];
    for param_node in node.children() {
        let AstNodePayload::KindParam(kind_param) = param_node.payload() else { continue };
        let name = parsing.string(kind_param.name).to_owned();
        let kind = match param_node.child(0).payload() {
            AstNodePayload::Kind(kind) => match parsing.string(kind.name).as_bytes() {
                b"Type" => Kind::Type,
                b"Nat" => Kind::Nat,
                _ => Kind::Type,
            },
            _ => continue,
        };
        params.push(TypeParam { name, kind });
    }
    params
}

/// The generic parameter list of the item owning `symbol`, if any.
/// Works for every item kind which accepts a `Generics` declaration
/// (typedefs, builtin defs, socket defs, mod defs).
pub(crate) fn params_of_item(builder: &mut Builder, symbol: &Symbol) -> Vec<TypeParam> {
    let location = symbol.location();
    let parsing = builder.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    match generics_decl_of(&node) {
        Some(generics_node) => parse_generics_decl(&generics_node),
        None => vec![],
    }
}

/// Check the number of written type arguments against the def's
/// parameter list, reporting `TypeArgCount` at `region` on mismatch.
/// Returns true when the count matches.
pub(crate) fn check_arg_count(
    params_len: usize,
    actual: usize,
    def_name: &BStr,
    region: crate::common::source::Region,
    diagnostics: &mut Vec<Diagnostic>,
) -> bool {
    if params_len != actual {
        diagnostics.push(Diagnostic::new(
            region,
            diagnostics::TypeArgCount {
                typ: def_name.to_owned(),
                expected: params_len,
                actual,
            },
        ));
        return false;
    }
    true
}

/// Report the `TypeArgKind` mismatch for one argument: a `Type`-kind
/// parameter given a `Nat` literal.
pub(crate) fn type_arg_kind_error(
    arg: &AstNode<'_>,
    def_name: &BStr,
    param_name: &BStr,
    expected: &str,
    actual: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    diagnostics.push(Diagnostic::new(
        arg.region(),
        diagnostics::TypeArgKind {
            typ: def_name.to_owned(),
            param: param_name.to_owned(),
            expected: expected.into(),
            actual: actual.into(),
        },
    ));
}

/// Interpret `arg` as a `Nat`-kind type argument, emitting the
/// kind-mismatch diagnostic when a type was written instead, or the
/// value-parse diagnostic when the literal is malformed.
pub(crate) fn nat_arg_value(
    arg: &AstNode<'_>,
    def_name: &BStr,
    param_name: &BStr,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<Width> {
    let parsing = arg.parsing();
    match arg.payload() {
        AstNodePayload::GenericsNat(nat) => {
            let spelling = parsing.string(nat.value).to_str_lossy().into_owned();
            match spelling.parse::<Width>() {
                Ok(width) => Some(width),
                Err(_) => {
                    diagnostics.push(Diagnostic::new(
                        arg.region(),
                        diagnostics::Unknown {
                            message: format!("Malformed width parameter: {spelling}").into(),
                        },
                    ));
                    None
                }
            }
        }
        _ => {
            type_arg_kind_error(arg, def_name, param_name, "a Nat", "a type", diagnostics);
            None
        }
    }
}

/// Resolve and kind-check the type arguments written on an
/// instantiation — a socket instance (`client socket p of Pipe[...]`)
/// or a submodule instance (`mod f of Fifo[...]`) — against the
/// instantiated def's generic parameters.
///
/// This is the instantiation-site half of the one shared
/// argument-checking routine (PLANv2 §10): type references are checked
/// in `gather_type_root`, socket and submodule instances here.  Arity
/// and kind errors are reported at the instantiation site.
///
/// The resolved arguments are the *mono* argument types for this
/// instance; inside a generic def they may themselves contain `Param`s
/// (generic analysis composes).
pub(crate) fn resolve_instantiation_args(
    builder: &mut Builder,
    args_node: Option<&AstNode<'_>>,
    def_symbol: &Symbol,
    region: crate::common::source::Region,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<Vec<Type>> {
    let params = params_of_item(builder, def_symbol);
    let arg_nodes: Vec<AstNode<'_>> = args_node
        .map(|args: &AstNode<'_>| args.children())
        .unwrap_or_default();
    if !check_arg_count(params.len(), arg_nodes.len(), def_symbol.fqn(), region, diagnostics) {
        return None;
    }

    let type_index = builder.get_type_index();
    let mut args: Vec<Type> = Vec::with_capacity(arg_nodes.len());
    for (param, arg) in params.iter().zip(arg_nodes.iter()) {
        match param.kind {
            Kind::Type => {
                if !matches!(arg.payload(), AstNodePayload::Type(_)) {
                    type_arg_kind_error(
                        arg,
                        def_symbol.fqn(),
                        param.name.as_bstr(),
                        "a type",
                        "a Nat literal",
                        diagnostics,
                    );
                    return None;
                }
                let Some(typ) = type_index.type_at(arg.location()).cloned() else {
                    // The inner type failed to resolve; its diagnostic
                    // is already reported.
                    return None;
                };
                args.push(typ);
            }
            Kind::Nat => {
                let Some(width) =
                    nat_arg_value(arg, def_symbol.fqn(), param.name.as_bstr(), diagnostics)
                else {
                    return None;
                };
                args.push(Type::Word(width));
            }
        }
    }
    Some(args)
}

/// Resolve the target def of a socket or submodule instantiation node
/// and its type arguments *as written*, without diagnostics (the
/// component analysis has already kind-checked them).  Returns `None`
/// if the target or arguments do not resolve (only possible on
/// unchecked input).  Inside a generic def the arguments may mention
/// that def's parameters (`Param`), to be composed by the caller.
pub(crate) fn instantiation_target(
    builder: &mut Builder,
    stmt: &AstNode<'_>,
) -> Option<(Symbol, Vec<Type>)> {
    let ofness_node = stmt
        .children()
        .iter()
        .find(|c| matches!(c.payload(), AstNodePayload::Ofness(_)))?
        .clone();
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        return None;
    };
    let parsing = builder.get_parsing(stmt.package());
    let packages = builder.get_packages();
    let target_package = match ofness.package {
        Some(pkg) => packages.id(parsing.string(pkg)),
        None => Some(stmt.package()),
    };
    let name = parsing.string(ofness.name);
    let symboltable = builder.get_symboltable();
    let symbol = target_package
        .and_then(|pkg| symboltable.resolve_item(name, pkg))?
        .clone();
    let args = instantiation_args(builder, stmt, &symbol)?;
    Some((symbol, args))
}

/// Resolve the type arguments written on an instantiation node against
/// the target def's parameter list, without diagnostics.
/// See `instantiation_target`.
fn instantiation_args(
    builder: &mut Builder,
    stmt: &AstNode<'_>,
    def_symbol: &Symbol,
) -> Option<Vec<Type>> {
    let args_node = stmt
        .children()
        .iter()
        .find(|c| matches!(c.payload(), AstNodePayload::GenericsParams(_)))
        .cloned();
    let params = params_of_item(builder, def_symbol);
    let arg_nodes: Vec<AstNode<'_>> = match &args_node {
        Some(node) => node.children(),
        None => vec![],
    };
    if arg_nodes.len() != params.len() {
        return None;
    }
    let type_index = builder.get_type_index();
    let mut args: Vec<Type> = Vec::with_capacity(arg_nodes.len());
    for arg in &arg_nodes {
        match arg.payload() {
            AstNodePayload::Type(_) => {
                args.push(type_index.type_at(arg.location())?.clone());
            }
            AstNodePayload::GenericsNat(nat) => {
                let spelling = arg.parsing().string(nat.value).to_str_lossy().into_owned();
                args.push(Type::Word(spelling.parse().ok()?));
            }
            _ => return None,
        }
    }
    Some(args)
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

        let params = params_of_item(builder, &item_symbol);

        let (width, enumerant_values) = if kind == TypeScheme::EnumDef {
            eval_enumdef(builder, &item_symbol)
        } else {
            (None, IndexMap::new())
        };

        typedefs.push(TypeDef {
            symbol_id: item_symbol.id(),
            kind,
            params,
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

    let params = params_of_item(builder, &item_symbol);

    let (width, enumerant_values) = if kind == TypeScheme::EnumDef {
        eval_enumdef(builder, &item_symbol)
    } else {
        (None, IndexMap::new())
    };

    Arc::new(TypeDef {
        symbol_id: item_symbol.id(),
        kind,
        params,
        width,
        enumerant_values,
    })
}

/// Evaluate the enumerant values of an enum type definition.
///
/// If no enumerant specifies a value, the enumerants are one-hot encoded
/// by default: enumerant `i` (in declaration order) gets the value `1 << i`,
/// and the width of the enum type is the number of enumerants.
fn eval_enumdef(builder: &mut Builder, item_symbol: &Symbol) -> (Option<Width>, IndexMap<SymbolId, WordValue>) {
    let location = item_symbol.location();
    let parsing = builder.get_parsing(location.package());
    let symboltable = builder.get_symboltable();
    let enumdef_node = parsing.ast_node(location.ast_node_id());

    // Collect (enumerant, optionally-specified value) in declaration order.
    let mut entries: Vec<(SymbolId, Option<WordValue>)> = vec![];
    let mut width = None;
    for enumerant_node in enumdef_node.children() {
        let AstNodePayload::Enumerant(enumerant) = enumerant_node.payload() else { continue; };
        let enumerant_name = parsing.string(enumerant.name);
        let Some(enumerant_symbol) = symboltable.slot(item_symbol.id(), enumerant_name) else { continue; };
        let enumerant_id = enumerant_symbol.id();
        // The value expression, if any, is the first expression child
        // (after the Annotations wrapper and any ctor Param children).
        let value = enumerant_node.children().into_iter().find(|child| child.is_expr()).map(|expr_node| {
            let (value, expr_width) = eval_const_expr(&expr_node);
            if width.is_none() {
                width = expr_width;
            }
            value
        });
        entries.push((enumerant_id, value));
    }

    let any_specified = entries.iter().any(|(_, value)| value.is_some());
    let mut enumerant_values = IndexMap::new();
    if !any_specified {
        // One-hot encoding: enumerant i gets 1 << i, width = number of enumerants.
        if let Ok(count) = u16::try_from(entries.len()) {
            width = Some(count);
        }
        for (index, (enumerant_id, _)) in entries.iter().enumerate() {
            let value = 1u64.checked_shl(index as u32).unwrap_or(0);
            enumerant_values.insert(*enumerant_id, value);
        }
    } else {
        for (enumerant_id, value) in entries {
            let value = value.unwrap_or_else(|| 1u64.checked_shl(enumerant_values.len() as u32).unwrap_or(0));
            enumerant_values.insert(enumerant_id, value);
        }
    }

    (width, enumerant_values)
}

fn eval_const_expr(node: &AstNode<'_>) -> (WordValue, Option<Width>) {
    let parsing = node.parsing;
    match node.payload() {
        AstNodePayload::ExprWordLit(expr_word_lit) => {
            let literal = parsing.string(expr_word_lit.literal).to_str_lossy().into_owned();
            let (value, width) = parse_word_literal(&literal);
            (value, width)
        }
        AstNodePayload::ExprParen => eval_const_expr(&node.child(0)),
        AstNodePayload::ExprAs => eval_const_expr(&node.child(0)),
        _ => panic!("Unsupported expression in enumerant value: {:?}", node.summary()),
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

    fn insert(&mut self, location: Location, typ: Type) {
        let idx = self.typs.insert_full(typ).0;
        self.typ_at_location.insert(location, TypeId(idx));
    }

    /// The `GenericsParams` child of a Type (or instantiation) node, if any.
    fn type_args_node<'p>(node: &'p AstNode<'p>) -> Option<AstNode<'p>> {
        node.children()
            .into_iter()
            .find(|child| matches!(child.payload(), AstNodePayload::GenericsParams(_)))
    }

    fn gather_type_roots(
        &mut self,
        builder: &mut Builder,
        node: AstNode<'_>,
        symboltable: &SymbolTable,
        // Generic parameters in scope: (name, kind), in declaration order.
        scope_params: &[(BString, Kind)],
    ) {
        if let AstNodePayload::Type(_) = node.payload() {
            self.gather_type_root(builder, &node, symboltable, scope_params);
            return;
        }

        // Entering an item which declares generic parameters establishes
        // a new scope for its body (struct fields, union ctor params,
        // enum enumerant params, socket channels, mod components).
        let owned_scope: Vec<(BString, Kind)>;
        let scope_params: &[(BString, Kind)] = if let Some(generics_node) = generics_decl_of(&node) {
            owned_scope = parse_generics_decl(&generics_node)
                .into_iter()
                .map(|param| (param.name, param.kind))
                .collect();
            &owned_scope
        } else {
            scope_params
        };

        for child in node.children() {
            self.gather_type_roots(builder, child, symboltable, scope_params);
        }
    }

    fn gather_type_root(
        &mut self,
        builder: &mut Builder,
        node: &AstNode<'_>,
        symboltable: &SymbolTable,
        scope_params: &[(BString, Kind)],
    ) {
        let parsing = node.parsing;
        let ofness = match node.child(0).payload() {
            AstNodePayload::Ofness(ofness) => ofness,
            _ => panic!("expected Ofness inside Type node"),
        };
        let type_name: BString = match ofness.package {
            Some(package) => {
                let package_name = parsing.string(package);
                let typ_name = parsing.string(ofness.name);
                format!("{}::{}", package_name, typ_name).into()
            }
            None => parsing.string(ofness.name).to_owned(),
        };

        // Process the argument Type nodes first, so their own types are
        // indexed (a Type node never recurses into its children).
        let args_node = Self::type_args_node(node);
        let arg_nodes: Vec<AstNode<'_>> = args_node
            .as_ref()
            .map(|args: &AstNode<'_>| args.children())
            .unwrap_or_default();
        for arg in &arg_nodes {
            if let AstNodePayload::Type(_) = arg.payload() {
                self.gather_type_roots(builder, arg.clone(), symboltable, scope_params);
            }
        }

        // A generic parameter reference (`T`), shadowing any item of the
        // same name.  Parameters take no arguments.
        if ofness.package.is_none() {
            let name = ofness.name;
            let param = parsing
                .string(name)
                .to_owned();
            if let Some((index, _)) = scope_params
                .iter()
                .enumerate()
                .find(|(_, (param_name, _))| *param_name == param)
            {
                if !arg_nodes.is_empty() {
                    self.diagnostics.push(Diagnostic::new(
                        node.region(),
                        diagnostics::Unknown {
                            message: format!("Parameter {param} takes no type arguments.").into(),
                        },
                    ));
                    return;
                }
                self.insert(node.location(), Type::Param(ParamId(index as u32)));
                return;
            }
        }

        let Some(symbol) = symboltable.resolve_item(type_name.as_bstr(), parsing.package()) else {
            self.diagnostics.push(Diagnostic::new(
                node.region(),
                diagnostics::UnresolvedType {
                    typ: type_name.into(),
                },
            ));
            return;
        };

        crate::queries::pubcheck::check_item_reference(
            builder,
            &symbol,
            parsing.package(),
            node.region(),
            &mut self.diagnostics,
        );

        let is_builtin = |name: &[u8]| {
            symboltable
                .resolve(name.into())
                .map_or(false, |b| b.id() == symbol.id())
        };

        let typ = if is_builtin(b"builtin::Bit") {
            if args_node.is_some() {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    diagnostics::Unknown {
                        message: "Type Bit takes no parameters.".into(),
                    },
                ));
                return;
            }
            Type::Bit
        } else if is_builtin(b"builtin::Clock") {
            if args_node.is_some() {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    diagnostics::Unknown {
                        message: "Type Clock takes no parameters.".into(),
                    },
                ));
                return;
            }
            Type::Clock
        } else if is_builtin(b"builtin::Reset") {
            if args_node.is_some() {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    diagnostics::Unknown {
                        message: "Type Reset takes no parameters.".into(),
                    },
                ));
                return;
            }
            Type::Reset
        } else if is_builtin(b"builtin::Word") {
            if arg_nodes.is_empty() {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    diagnostics::Unknown {
                        message: "Word type requires a width parameter.".into(),
                    },
                ));
                return;
            }
            if arg_nodes.len() > 1 {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    diagnostics::TypeArgCount {
                        typ: type_name.into(),
                        expected: 1,
                        actual: arg_nodes.len(),
                    },
                ));
                return;
            }
            let Some(width) = nat_arg_value(&arg_nodes[0], type_name.as_bstr(), b"n".as_bstr(), &mut self.diagnostics) else {
                return;
            };
            Type::Word(width)
        } else {
            // A user-defined (or library) type: kind-check the
            // arguments against the def's declared parameters, then
            // carry them on the type.
            let params: Vec<TypeParam> = match symbol.kind() {
                SymbolKind::UnionDef
                | SymbolKind::StructDef
                | SymbolKind::EnumDef
                | SymbolKind::BuiltinDef => params_of_item(builder, &symbol),
                _ => vec![],
            };

            if !check_arg_count(
                params.len(),
                arg_nodes.len(),
                type_name.as_bstr(),
                node.region(),
                &mut self.diagnostics,
            ) {
                return;
            }

            let mut args: Vec<Type> = Vec::with_capacity(arg_nodes.len());
            for (param, arg) in params.iter().zip(arg_nodes.iter()) {
                match param.kind {
                    Kind::Type => {
                        if !matches!(arg.payload(), AstNodePayload::Type(_)) {
                            type_arg_kind_error(
                                arg,
                                type_name.as_bstr(),
                                param.name.as_bstr(),
                                "a type",
                                "a Nat literal",
                                &mut self.diagnostics,
                            );
                            return;
                        }
                        let Some(arg_typ) = self.type_at(arg.location()).cloned() else {
                            // The inner type failed to resolve; its
                            // diagnostic is already reported.
                            return;
                        };
                        args.push(arg_typ);
                    }
                    Kind::Nat => {
                        let Some(width) = nat_arg_value(
                            arg,
                            type_name.as_bstr(),
                            param.name.as_bstr(),
                            &mut self.diagnostics,
                        ) else {
                            return;
                        };
                        args.push(Type::Word(width));
                    }
                }
            }
            Type::Usual(symbol.id(), args)
        };

        self.insert(node.location(), typ);
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
    for package in packages.ids() {
        let parsing = builder.get_parsing(package);
        type_index.gather_type_roots(builder, parsing.root(), &symboltable, &[]);
    }

    Arc::new(type_index)
}
