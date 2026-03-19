use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use hashbrown::{HashMap, HashSet};

use crate::analysis::Location;
use crate::analysis::symboltable::{SymbolId, SymbolKind};
use crate::common::{BinOp as CommonBinOp, ComponentKind, TypeScheme, UnOp as CommonUnOp, Width};
use crate::common::json::ToJson;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

macro_rules! error {
    ($self_:expr, $node:expr, $fmt:literal) => {{
        error!($self_, $node, $fmt,)
    }};
    ($self_:expr, $node:expr, $fmt:literal, $($arg:expr),*) => {{
        $self_.diagnostics.push(diagnostics::Unknown {
            region: $node.region(),
            message: format!($fmt, $($arg)*).into(),
        }.into());
    }};
}

macro_rules! info {
    ($self_:expr, $node:expr, $fmt:literal) => {{
        error!($self_, $node, $fmt,)
    }};
    ($self_:expr, $node:expr, $fmt:literal, $($arg:expr),*) => {{
        $self_.diagnostics.push(diagnostics::Todo {
            region: $node.region(),
            message: format!($fmt, $($arg)*).into(),
        }.into());
    }};
}

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

    pub fn get(&self, name: BString) -> Option<Type> {
        for (name_, typ) in self.bindings().iter().rev() {
            if name == *name_ {
                return Some(typ.clone());
            }
        }
        None
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
                let module = symboltable.resolve_item_in_package(module_name, module_package);
                if module.is_none() {
                    continue;
                }
                module.unwrap()
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
            component_analysis.type_of(lhs_path)
        }
        AstNodePayload::ModDefStmtOn => {
            Some(Type::Clock)
        }
        AstNodePayload::CommandAssert => {
            Some(Type::Bit)
        }
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
    let exprroot_ids: HashSet<_> = builder
        .get_exprroots()
        .into_iter()
        .map(|exprroot| exprroot.location().ast_node_id())
        .collect();
    let mut node = parsing.ast_node(location.ast_node_id());
    loop {
        if exprroot_ids.contains(&node.id()) {
            return ExprRoot::new(node.location());
        }

        let parent_id = node
            .parent()
            .expect("expected node to be contained in an expr root")
            .id();
        node = parsing.ast_node(parent_id);
        &node;
    }
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

    fn flag_wrong_type<'p>(&mut self, node: &AstNode<'p>, expected: &Type, actual: &Type) {
        let diag: Diagnostic = diagnostics::WrongType {
            region: node.region(),
            expected: expected.to_string().into(),
            actual: actual.to_string().into(),
        }.into();
        self.diagnostics.push(diag.to_warning());
    }

    fn flag_not_word_type<'p>(&mut self, node: &AstNode<'p>, typ: &Type) {
        self.diagnostics.push(diagnostics::NotWordType {
            region: node.region(),
            typ: typ.to_string().into(),
        }.into());
    }

    fn flag_cant_infer<'p>(&mut self, node: &AstNode<'p>) {
        self.diagnostics.push(diagnostics::CantInfer {
            region: node.region(),
        }.into());
    }

    fn flag_unresolved_component<'p>(&mut self, node: &AstNode<'p>, path: &BStr) {
        self.diagnostics.push(diagnostics::UnresolvedComponent {
            region: node.region(),
            path: path.into(),
        }.into());
    }

    fn check<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        match self.infer(node) {
            Err(diagnostic) => {
                self.diagnostics.push(diagnostic);
                return;
            }
            Ok(Some(actual_typ)) => {
                if actual_typ == *expected_typ {
                    self.typs.insert(node.id(), actual_typ);
                } else {
                    self.flag_wrong_type(node, expected_typ, &actual_typ);
                }
                return;
            }
            _ => (),
        }

        match node.payload() {
            AstNodePayload::ExprParen => self.check_paren(node, expected_typ),
            AstNodePayload::ExprIf => self.check_if(node, expected_typ),
            AstNodePayload::ExprMatch => (), // TODO
            AstNodePayload::ExprWordLit(_) => self.check_word_lit(node, expected_typ),
            AstNodePayload::ExprBinOp(expr_bin_op) => self.check_binop(node, expr_bin_op.op, expected_typ),
            AstNodePayload::ExprUnOp(expr_un_op) => self.check_unop(node, expr_un_op.op, expected_typ),
            AstNodePayload::ExprMethod(expr_method) => (), // TODO
            AstNodePayload::ExprFn => (), // TODO
            AstNodePayload::ExprCtor(expr_ctor) => (), // TODO
            AstNodePayload::ExprEnumerant(expr_enumerant) => (), // TODO
            AstNodePayload::ExprStruct => (), // TODO
            AstNodePayload::ExprIndex(expr_index) => self.check_index(node, expr_index.index, expected_typ),
            AstNodePayload::ExprIndexRange(expr_index_range) => (), // TODO
            AstNodePayload::ExprWord => self.check_word(node, expected_typ),
            AstNodePayload::ExprZext | AstNodePayload::ExprSext => self.check_ext(node, expected_typ),
            _ => unreachable!("Can't typecheck {:?}", node.summary()),
        }
    }

    fn check_paren<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        let child = node.child(0);
        self.check(&child, expected_typ);
        if self.typs.contains_key(&child.id()) {
            self.typs.insert(node.id(), expected_typ.clone());
        }
    }

    fn check_if<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        let children = node.children();
        self.check(&children[0], &Type::Bit);
        self.check(&children[1], expected_typ);
        let mut i = 2;
        while i + 1 < children.len() - 1 {
            self.check(&children[i], &Type::Bit);
            self.check(&children[i + 1], expected_typ);
            i += 2;
        }

        // TODO this all needs helpers in AstNode
        self.check(&children[children.len() - 1], expected_typ);

        if children.iter().all(|child| self.typs.contains_key(&child.id())) {
            self.typs.insert(node.id(), expected_typ.clone());
        }
    }

    fn check_word_lit<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        let Type::Word(width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return;
        };

        let (value, explicit_width) = parse_word_literal(node.spelling().to_str_lossy().as_ref());
        if let Some(explicit_width) = explicit_width {
            let actual = Type::Word(explicit_width);
            if actual == *expected_typ {
                self.typs.insert(node.id(), actual);
            } else {
                self.flag_wrong_type(node, expected_typ, &actual);
            }
            return;
        }

        let minwidth = min_word_width(value);
        if minwidth > u64::from(*width) {
            self.diagnostics.push(diagnostics::DoesntFit {
                region: node.region(),
                value,
                width: u64::from(*width),
                minwidth,
            }.into());
            return;
        }

        self.typs.insert(node.id(), Type::Word(*width));
    }

    fn check_binop<'p>(&mut self, node: &AstNode<'p>, op: CommonBinOp, expected_typ: &Type) {
        let lhs = node.child(0);
        let rhs = node.child(1);
        let lhs_typ = self.infer(&lhs).unwrap();
        let rhs_typ = self.infer(&rhs).unwrap();

        if matches!((&lhs_typ, &rhs_typ), (&None, &None)) {
            self.flag_cant_infer(node);
            return;
        }

        let lhs_typ = lhs_typ.unwrap();
        let rhs_typ = rhs_typ.unwrap();

        self.typs.insert(lhs.id(), lhs_typ.clone());
        self.typs.insert(rhs.id(), rhs_typ.clone());

        match op {
            CommonBinOp::Add | CommonBinOp::Sub => {
                if lhs_typ != rhs_typ {
                    self.flag_wrong_type(&rhs, &lhs_typ, &rhs_typ);
                    return;
                }
                if lhs_typ == *expected_typ {
                    self.typs.insert(node.id(), lhs_typ);
                } else {
                    self.flag_wrong_type(node, expected_typ, &lhs_typ);
                }
            }
            CommonBinOp::Lt
            | CommonBinOp::Lte
            | CommonBinOp::Gt
            | CommonBinOp::Gte
            | CommonBinOp::Eq
            | CommonBinOp::Neq => {
                if lhs_typ != rhs_typ {
                    self.flag_wrong_type(&rhs, &lhs_typ, &rhs_typ);
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
            CommonBinOp::And | CommonBinOp::Or | CommonBinOp::Xor => {
                if lhs_typ != Type::Bit {
                    self.flag_wrong_type(&lhs, &Type::Bit, &lhs_typ);
                    return;
                }
                if rhs_typ != Type::Bit {
                    self.flag_wrong_type(&rhs, &Type::Bit, &rhs_typ);
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
        }
    }

    fn check_unop<'p>(&mut self, node: &AstNode<'p>, op: CommonUnOp, expected_typ: &Type) {
        let rhs = node.child(0);
        let Ok(Some(rhs_typ)) = self.infer(&rhs) else {
            self.flag_cant_infer(node);
            return;
        };
        self.typs.insert(rhs.id(), rhs_typ.clone());

        match op {
            CommonUnOp::Neg | CommonUnOp::Inv => {
                if rhs_typ == *expected_typ {
                    self.typs.insert(node.id(), rhs_typ);
                } else {
                    self.flag_wrong_type(node, expected_typ, &rhs_typ);
                }
            }
            CommonUnOp::Not => {
                if rhs_typ != Type::Bit {
                    self.flag_wrong_type(&rhs, &Type::Bit, &rhs_typ);
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
        }
    }

    fn check_index<'p>(&mut self, node: &AstNode<'p>, index: Width, expected_typ: &Type) {
        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(&subject) else {
            self.flag_cant_infer(node);
            return;
        };

        self.typs.insert(subject.id(), subject_typ.clone());

        match subject_typ {
            Type::Word(width) => {
                if index >= width {
                    self.diagnostics.push(diagnostics::Unknown {
                        region: node.region(),
                        message: format!("Index {index} out of bounds for Word[{width}]").into(),
                    }.into());
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
            other => self.flag_not_word_type(&subject, &other),
        }
    }

    fn check_word<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        // We can assume we have a width-less WordLit here, since it would have been inferred earlier.
        let mut width: Width = todo!();

        let actual = Type::Word(width);
        if actual == *expected_typ {
            self.typs.insert(node.id(), actual);
        } else {
            self.flag_wrong_type(node, expected_typ, &actual);
        }
    }

    fn check_ext<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        todo!()
        /*
        let Type::Word(target_width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return;
        };

        let subject = node.child(0);
        let Some(subject_typ) = self.infer(&subject) else {
            self.flag_cant_infer(node);
            return;
        };

        let subject_width = match subject_typ {
            Type::Bit | Type::Clock => 1,
            Type::Word(width) => width,
            other => {
                self.flag_not_word_type(&subject, &other);
                return;
            }
        };

        if subject_width > *target_width {
            self.flag_wrong_type(node, &Type::Word(subject_width), expected_typ);
            return;
        }

        self.typs.insert(node.id(), expected_typ.clone());
        */
    }

    fn infer<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, Diagnostic> {
        match node.payload() {
            AstNodePayload::ExprParen => self.infer(&node.child(0)),
            AstNodePayload::ExprReference => {
                // TODO HACK
                let parsing = node.parsing();
                let path = parsing.string(node.path().unwrap());
                let typ = self.context.get(path.to_owned()); // TODO need to walk backwards to get it.
                if typ.is_none() {
                    Err(diagnostics::UnresolvedComponent {
                        region: node.region(),
                        path: path.into(),
                    }.into())
                } else {
                    Ok(typ)
                }
            }
            AstNodePayload::ExprBitLit(expr_bit_lit) => {
                Ok(Some(Type::Bit))
            }
            AstNodePayload::ExprWordLit(expr_word_lit) => {
                // TODO I shouldn't be doing string manip here.

                let path = node.spelling();
                if path.contains(&b'w') {
                    let parts: Vec<_> = path.split(|ch| *ch == b'w').collect();
                    let width: Width = std::str::from_utf8(parts[1]).unwrap().parse().unwrap();
                    Ok(Some(Type::Word(width)))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }
}

fn parse_word_literal(literal: &str) -> (u64, Option<Width>) {
    if let Some((value, width)) = literal.split_once('w') {
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

fn parse_nat_literal(literal: &str) -> u64 {
    let literal = literal.replace('_', "");
    if let Some(hex) = literal.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap()
    } else if let Some(bin) = literal.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap()
    } else {
        literal.parse().unwrap()
    }
}

fn min_word_width(value: u64) -> u64 {
    if value == 0 {
        0
    } else {
        u64::BITS as u64 - u64::leading_zeros(value) as u64
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
