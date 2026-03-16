use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use hashbrown::{HashMap, HashSet};

use crate::analysis::Location;
use crate::analysis::symboltable::{SymbolId, SymbolKind};
use crate::common::{ComponentKind, TypeScheme, Width};
use crate::common::json::ToJson;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDef {
    fqn: BString,
    location: Location,
    kind: TypeScheme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    pub location: Location,
}

#[derive(Debug)]
pub struct Typing {
    expr_root: ExprRoot,
    expected_typ: Type,
    context: TypingContext,
    typs: HashMap<AstNodeId, Type>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bit,
    Clock,
    Word(Width),
    Usual(SymbolId), // TODO rename this
}

#[derive(Debug, Clone)]
pub struct TypingContext(Vec<(BString, Type)>);

impl TypingContext {
    pub fn bindings(&self) -> &[(BString, Type)] {
        self.0.as_slice()
    }

    pub fn get(&self, name: BString) -> Type {
        for (name_, typ) in self.bindings().iter().rev() {
            if name == *name_ {
                return typ.clone();
            }
        }
        panic!("No binding found for {name}")
    }
}

impl ExprRoot {
    pub fn new(location: Location) -> ExprRoot {
        ExprRoot { location }
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }
}

pub fn build_typedefs(builder: &mut Builder) -> Vec<TypeDef> {
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

pub fn build_typing_context(builder: &mut Builder, item: SymbolId) -> TypingContext {
    let symboltable = builder.get_symboltable();

    let symbol = symboltable.symbol(item);
    let location = symbol.location();
    let parsing = builder.get_parsing(location.package());

    let item_ast = parsing.ast_node(location.ast_node_id());
    let component_analysis = builder.get_component_analysis(item);

    // TODO How does this need to get used?
    let typedefs = builder.get_typedefs();


    // TODO HACK this isn't complete
    let mut context = TypingContext(vec![]);
    for (path, opt_typ) in component_analysis.components() {
        if let Some(typ) = opt_typ {
            context.0.push((path, typ));
        }
    }

    for stmt in item_ast.children() {
        let AstNodePayload::Module(module) = stmt.payload() else {
            continue;
        };

        let instance_name = parsing.string(module.name);
        let module = match stmt.child(0).payload() {
            AstNodePayload::Ofness(ofness) => {
                let module_package = ofness
                    .package
                    .map(|package| PackageFqn::new(BString::from(parsing.string(package).to_vec())))
                    .unwrap_or_else(|| location.package());
                let module_name = parsing.string(ofness.name);
                symboltable.resolve_item_in_package(module_name, module_package).unwrap()
            }
            _ => todo!(),
        };


        let module_component_analysis = builder.get_component_analysis(module.id());
        for (port_name, opt_typ) in module_component_analysis.components() {
            let qualified_name =
                BString::from(format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes());
            if let Some(typ) = opt_typ {
                context.0.push((qualified_name, typ));
            }
        }
    }

    context
}

pub fn build_exprroots(builder: &mut Builder) -> Vec<ExprRoot> {
    let mut exprroots = vec![];
    for package in builder.get_packages() {
        let analysis = builder.get_package_analysis(package);

        for ast_node_id in analysis.expr_roots_node_ids() {
            let location = Location::new(analysis.package(), ast_node_id);
            exprroots.push(ExprRoot::new(location));
        }
    }
    exprroots
}

pub fn build_expected_type(builder: &mut Builder, location: Location) -> Option<Type> {
    let parsing = builder.get_parsing(location.package());
    let symboltable = builder.get_symboltable();

    let node = parsing.ast_node(location.ast_node_id());
    let parent_node = node.parent().unwrap();

    match parent_node.payload() {
        AstNodePayload::Component(component) if component.kind == ComponentKind::Reg => Some(Type::Clock),
        AstNodePayload::Driver(_) => {
            let lhs_path = parsing.string(parent_node.child(0).path().unwrap());

            let moddef_node = parent_node.parent().unwrap();
            let moddef_name = parsing.string(moddef_node.name().unwrap());
            let moddef = symboltable
                .resolve_item_in_package(moddef_name, location.package())
                .unwrap();
            let component_analysis = builder.get_component_analysis(moddef.id());

            if let Some(dot_index) = lhs_path.iter().position(|ch| *ch == b'.') {
                let instance_name = BStr::new(&lhs_path[..dot_index]);
                let port_name = BStr::new(&lhs_path[(dot_index + 1)..]);

                let instance_node = moddef_node
                    .children()
                    .into_iter()
                    .find(|child| {
                        matches!(child.payload(), AstNodePayload::Module(module)
                            if parsing.string(module.name) == instance_name)
                    })
                    .unwrap_or_else(|| panic!("Couldn't find instance {lhs_path}"));

                let module = match instance_node.child(0).payload() {
                    AstNodePayload::Ofness(ofness) => {
                        let module_package = ofness
                            .package
                            .map(|package| PackageFqn::new(BString::from(parsing.string(package).to_vec())))
                            .unwrap_or_else(|| location.package());
                        let module_name = parsing.string(ofness.name);
                        symboltable.resolve_item_in_package(module_name, module_package).unwrap()
                    }
                    _ => todo!(),
                };

                let module_component_analysis = builder.get_component_analysis(module.id());
                if port_name == b"clock" {
                    Some(Type::Clock)
                } else {
                    module_component_analysis.type_of(port_name)
                }
            } else if lhs_path == b"clock" {
                Some(Type::Clock)
            } else {
                component_analysis.type_of(lhs_path)
            }
        },
        _ => todo!("Can't build expected type for: {:?}", parent_node.summary()),
    }
}

pub fn typecheck(builder: &mut Builder) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for exprroot in builder.get_exprroots() {
        let typing = builder.get_typing(exprroot);
        diagnostics.extend(typing.diagnostics());
    }

    diagnostics
}

pub fn build_typeof(builder: &mut Builder, location: Location) -> Option<Type> {
    let exprroot = exprroot_anscestor(builder, location.clone());
    let typing = builder.get_typing(exprroot);
    let typ = typing.typs.get(&location.ast_node_id());
    typ.cloned()
}

fn exprroot_anscestor(builder: &mut Builder, location: Location) -> ExprRoot {
    let parsing = builder.get_parsing(location.package());
    let mut node = parsing.ast_node(location.ast_node_id());
    while node.id() != location.ast_node_id() {
        let parent_id = node.parent().unwrap().id();
        node = parsing.ast_node(parent_id);
    }
    ExprRoot::new(location)
}

pub fn build_typing(builder: &mut Builder, expr_root: ExprRoot) -> Arc<Typing> {
    let location = expr_root.location();
    let parsing = builder.get_parsing(location.package());

    let mut current_id = location.ast_node_id();
    // REVIEW walk up until you find the containing item ast node.
    let item_name = loop {
        let current = parsing.ast_node(current_id);
        if current.is_item() {
            break parsing
                .string(current.name().expect("expected containing item to have a name"))
                .to_owned();
        }
        current_id = current
            .parent()
            .expect("expected expr root to be contained in an item")
            .id();
    };
    let symboltable = builder.get_symboltable();
    let item = symboltable
        .resolve_item_in_package(item_name.as_bstr(), location.package())
        .unwrap();
    let context = builder.get_typing_context(item.id());

    let node = parsing.ast_node(location.ast_node_id());
    let expected_typ = builder.get_expected_type(location.clone());
    let fallback_expected_typ = expected_typ.clone();
    let expected_typ = expected_typ.unwrap_or_else(|| builtin_bit_type(builder));

    let diagnostics = vec![];
    let mut typing = Typing {
        expr_root: expr_root.clone(),
        context,
        typs: HashMap::new(),
        diagnostics,
        expected_typ: expected_typ.clone(),
    };

    // if there is no expected type, you can't type check the expression.
    if fallback_expected_typ.is_some() {
        typing.check(&node, &expected_typ);
    } else {
        typing.diagnostics.push(diagnostics::Todo {
            region: node.region(),
            message: format!("Can't typecheck expression because we don't know what type it should have").into(),
        }.into());
    }

    Arc::new(typing)
}

fn builtin_bit_type(builder: &mut Builder) -> Type {
//    TODO
//    let symboltable = builder.get_symboltable();
//    let bit_symbol = symboltable.resolve("builtin::Bit".into()).unwrap();
//    let _ = bit_symbol;
    Type::Bit
}

pub fn build_type_monomorphizations(builder: &mut Builder) -> Vec<Type> {
    let mut typs = HashSet::new();

    builder.typecheck();

    for exprroot in builder.get_exprroots() {
        let typing = builder.get_typing(exprroot);
        for typ in typing.typs.values() {
            typs.insert(typ.clone());
        }
    }

    typs.into_iter().collect()
}

impl Typing {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    fn check<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        if let Some(actual_typ) = self.infer(node) {
            if actual_typ == *expected_typ {
                self.typs.insert(node.id(), actual_typ);
            } else {
                let diag: Diagnostic = diagnostics::WrongType {
                    region:  node.region(),
                    expected: expected_typ.to_string().into(),
                    actual: actual_typ.to_string().into(),
                }.into();

                self.diagnostics.push(diag.to_warning());
            }
            return;
        }

        match node.payload() {
            AstNodePayload::ExprParen => todo!(),
            AstNodePayload::ExprIf => todo!(),
            AstNodePayload::ExprMatch => todo!(),
            AstNodePayload::ExprWordLit(expr_word_lit) => todo!(),
            AstNodePayload::ExprBinOp(expr_bin_op) => todo!(),
            AstNodePayload::ExprUnOp(expr_un_op) => todo!(),
            AstNodePayload::ExprMethod(expr_method) => todo!(),
            AstNodePayload::ExprFn => todo!(),
            AstNodePayload::ExprCtor(expr_ctor) => todo!(),
            AstNodePayload::ExprEnumerant(expr_enumerant) => todo!(),
            AstNodePayload::ExprStruct => todo!(),
            AstNodePayload::ExprIndex(expr_index) => todo!(),
            AstNodePayload::ExprIndexRange(expr_index_range) => todo!(),
            AstNodePayload::ExprWord => todo!(),
            AstNodePayload::ExprZext => todo!(),
            AstNodePayload::ExprSext => todo!(),
            _ => unreachable!(),
        }
    }

    fn infer<'p>(&mut self, node: &AstNode<'p>) -> Option<Type> {
        match node.payload() {
            AstNodePayload::ExprReference => {
                // TODO HACK
                let parsing = node.parsing();
                let path = parsing.string(node.path().unwrap());
                let typ = self.context.get(path.to_owned()); // TODO need to walk backwards to get it.
                Some(typ)
            }
            AstNodePayload::ExprBitLit(expr_bit_lit) => {
                Some(Type::Bit)
            }
            AstNodePayload::ExprWordLit(expr_word_lit) => {
                // TODO I shouldn't be doing string manip here.

                let path = node.spelling();
                if path.contains(&b'w') {
                    let parts: Vec<_> = path.split(|ch| *ch == b'w').collect();
                    let width: Width = std::str::from_utf8(parts[1]).unwrap().parse().unwrap();
                    Some(Type::Word(width))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl ToJson for Typing {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}

impl ToJson for TypingContext {
    fn to_json(&self) -> json::JsonValue {
        self.0.to_json()
    }
}

impl ToJson for ExprRoot {
    fn to_json(&self) -> json::JsonValue {
        self.location.to_json()
    }
}

impl ToJson for TypeDef {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}

impl ToJson for Type {
    fn to_json(&self) -> json::JsonValue {
        self.to_string().into()
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bit => write!(f, "Bit"),
            Type::Clock => write!(f, "Clock"),
            Type::Word(n) => write!(f, "Word[{n}]"),
            Type::Usual(symbol_id) => write!(f, "{self:?}"),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bit => write!(f, "Bit"),
            Type::Clock => write!(f, "Clock"),
            Type::Word(n) => write!(f, "Word[{n}]"),
            Type::Usual(symbol_id) => write!(f, "Usual({symbol_id:?})"),
        }
    }
}
