#![allow(dead_code)]
use std::sync::Arc;
use bstr::{BString, ByteSlice};

use crate::analysis::component::ComponentId;
use crate::analysis::drivers::{Driver, DriverWhen, DriverMatch};
use crate::common::WordValue;
use crate::db::Db;
use crate::sim::payload;
use crate::syntax::ast::{AstNode, match_arm_children};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;
use crate::types::substitute::substitute;
use crate::types::Type;
use crate::analysis::Location;

#[derive(Debug)]
pub struct Expr {
    location: Location,
    typ: Type,
    payload: ExprPayload,
}

#[derive(Debug)]
pub enum ExprPayload {
    Reference(payload::Reference),
    Paren(payload::Paren),
    When(payload::When),
    Match(payload::Match),
    BitLit(payload::BitLit),
    WordLit(payload::WordLit),
    StrLit(payload::StrLit),
    BinOp(payload::BinOp),
    UnOp(payload::UnOp),
    Method(payload::Method),
    Fn(payload::Fn),
    Ctor(payload::Ctor),
    Enumerant(payload::Enumerant),
    Struct(payload::Struct),
    Index(payload::Index),
    IndexDyn(payload::IndexDyn),
    IndexRange(payload::IndexRange),
    Word(payload::Word),
    Zext(payload::Zext),
    Sext(payload::Sext),
    AsClock(payload::AsClock),
    Cast(payload::Cast),
    Trunc(payload::Trunc),
    Any(payload::Any),
    All(payload::All),
    As(payload::As),
    Hole(payload::Hole),
    Dontcare(payload::Dontcare),
}

impl Expr {
    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn typ(&self) -> &Type {
        &self.typ
    }

    pub fn payload(&self) -> &ExprPayload {
        &self.payload
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Referent {
    Component(ComponentId),
    Location(Location),
    /// The component that owns the driver expression being evaluated:
    /// used for `when` drivers with no matching clause / no else clause
    /// ("hold the current value" — the X-free stand-in for Verilog's
    /// latched/inert semantics).
    Owner,
}

/// Convert a driver tree to a sim `Expr`.  `env` holds the mono type
/// arguments of the generic mod def instance the driver belongs to
/// (`[]` when its module is non-generic); every type fetched from the
/// (generic) analysis is substituted with them.  `component_typ` is
/// the driven component's mono type: the converted driver is tagged
/// with it.
pub fn driver_to_expr(db: &Db, driver: &Driver, component_typ: &Type, env: &[Type]) -> Arc<Expr> {
    match driver {
        Driver::Expr(_, loc) => convert_ast_expr(db, loc.clone(), env),
        Driver::Bidirectional(_) => unreachable!("Bidirectional drivers are resolved into aliases, not simulation"),
        Driver::Absent => unreachable!("Absent drivers only appear inside when clauses"),
        Driver::When(driver_when) => convert_driver_when(db, driver_when, component_typ, env),
        Driver::Match(driver_match) => convert_driver_match(db, driver_match, component_typ, env),
    }
}

/// Convert the RHS of an `initial` statement (at `loc`) into a sim
/// `Expr`.  `env` holds the mono type arguments of the owning instance.
pub fn initial_to_expr(db: &Db, loc: &Location, env: &[Type]) -> Arc<Expr> {
    convert_ast_expr(db, loc.clone(), env)
}

fn convert_driver_match(db: &Db, dm: &DriverMatch, component_typ: &Type, env: &[Type]) -> Arc<Expr> {
    let location = dm.subject.clone();
    let subject = convert_ast_expr(db, dm.subject.clone(), env);
    let subject_typ = mono_type(db, &dm.subject.clone(), env);
    let parsing = db.get_parsing(dm.subject.package());
    let mut arms: Vec<(payload::Pat, Arc<Expr>)> = Vec::new();
    for (pat_loc, sub_driver) in &dm.arms {
        let pat_node = parsing.ast_node(pat_loc.ast_node_id());
        let pattern = convert_pattern(db, &parsing, &pat_node, &subject_typ);
        let body = driver_to_expr(db, sub_driver.as_ref(), component_typ, env);
        arms.push((pattern, body));
    }
    if let Some(else_driver) = &dm.else_clause {
        let body = driver_to_expr(db, else_driver.as_ref(), component_typ, env);
        arms.push((payload::Pat::Else, body));
    } else {
        // No else arm: un-matched subjects hold their current value.
        arms.push((payload::Pat::Else, self_reference(location.clone(), component_typ)));
    }
    Arc::new(Expr {
        location,
        typ: component_typ.clone(),
        payload: ExprPayload::Match(payload::Match { subject, arms }),
    })
}

/// A reference to the owning component itself (`Referent::Owner`), tagged
/// with the component's type.
fn self_reference(location: Location, component_typ: &Type) -> Arc<Expr> {
    Arc::new(Expr {
        location,
        typ: component_typ.clone(),
        payload: ExprPayload::Reference(payload::Reference { referent: Referent::Owner }),
    })
}

fn convert_pattern(
    db: &Db,
    parsing: &Parsing,
    pat_node: &AstNode<'_>,
    subject_typ: &Type,
) -> payload::Pat {
    match pat_node.payload() {
        AstNodePayload::PatCtor(pat_ident) => {
            let ctor_name = parsing.string(pat_ident.name);
            let Type::Usual(typedef_id, _args) = subject_typ else {
                return payload::Pat::Else;
            };
            let symboltable = db.get_symboltable();
            let Some(ctor_sym) = symboltable.slot(*typedef_id, ctor_name) else {
                return payload::Pat::Else;
            };
            let bound_vars: Vec<(BString, Location)> = pat_node.children().iter()
                .filter_map(|c| {
                    let interned = c.path()?;
                    Some((parsing.string(interned).to_owned(), c.location()))
                })
                .collect();
            payload::Pat::Ctor { symbol_id: ctor_sym.id(), bound_vars }
        }
        AstNodePayload::PatEnumerant(pat_enum) => {
            let Type::Usual(typedef_id, _args) = subject_typ else {
                return payload::Pat::Else;
            };
            let symboltable = db.get_symboltable();
            let enum_name = parsing.string(pat_enum.name);
            let Some(sym) = symboltable.slot(*typedef_id, enum_name) else {
                return payload::Pat::Else;
            };
            payload::Pat::Ctor { symbol_id: sym.id(), bound_vars: vec![] }
        }
        AstNodePayload::PatWordLit(pat_word_lit) => {
            let Type::Word(width) = subject_typ else {
                return payload::Pat::Else;
            };
            let literal = parsing.string(pat_word_lit.literal).to_str_lossy().into_owned();
            let value = parse_word_value(&literal);
            payload::Pat::WordLit { width: *width, value }
        }
        AstNodePayload::PatBitLit(pat_bit_lit) => {
            if !matches!(subject_typ, Type::Bit | Type::Reset) {
                return payload::Pat::Else;
            }
            payload::Pat::BitLit { value: pat_bit_lit.literal }
        }
        _ => payload::Pat::Else,
    }
}

fn convert_driver_when(db: &Db, driver_when: &DriverWhen, component_typ: &Type, env: &[Type]) -> Arc<Expr> {
    let location = match driver_when.clauses.first() {
        Some((cond_loc, _)) => cond_loc.clone(),
        None => driver_when.else_clause.as_ref()
            .and_then(|else_driver| else_driver.location())
            .expect("when driver has no clauses and no else clause"),
    };

    let mut branches: Vec<(Arc<Expr>, Arc<Expr>)> = Vec::new();
    for (cond_loc, sub_driver) in &driver_when.clauses {
        let cond = convert_ast_expr(db, cond_loc.clone(), env);
        let body = match sub_driver.as_ref() {
            // The component is not driven in this clause: it holds its
            // current value (the X-free stand-in for Verilog's inert
            // sequential/latch semantics).
            Driver::Absent => self_reference(cond_loc.clone(), component_typ),
            sub_driver => driver_to_expr(db, sub_driver, component_typ, env),
        };
        branches.push((cond, body));
    }

    let else_branch = match &driver_when.else_clause {
        Some(else_driver) => driver_to_expr(db, else_driver.as_ref(), component_typ, env),
        // No else clause: the component holds its current value.
        None => self_reference(location.clone(), component_typ),
    };

    let typ = else_branch.typ.clone();
    Arc::new(Expr { location, typ, payload: ExprPayload::When(payload::When { branches, else_branch }) })
}

// Recursively build `If` branches from the flat [cond, then, cond, then, ..., else] child list.
fn build_if_branches(db: &Db, locs: &[Location], env: &[Type]) -> (Vec<(Arc<Expr>, Arc<Expr>)>, Arc<Expr>) {
    let cond = convert_ast_expr(db, locs[0].clone(), env);
    let then_expr = convert_ast_expr(db, locs[1].clone(), env);
    if locs.len() == 3 {
        let else_expr = convert_ast_expr(db, locs[2].clone(), env);
        (vec![(cond, then_expr)], else_expr)
    } else {
        let (mut rest, else_expr) = build_if_branches(db, &locs[2..], env);
        rest.insert(0, (cond, then_expr));
        (rest, else_expr)
    }
}

/// Walk up from `start_id` looking for an enclosing `ModDefStmtMatch` or
/// `ExprMatch` arm whose pattern binds a variable named `name`.  Returns the
/// `Location` of that pattern variable's AST node, which `eval_match` later
/// uses as the `Referent::Location` key when pushing the bound value into the
/// evaluation context.
fn resolve_pattern_binding(
    parsing: &Parsing,
    start_id: crate::syntax::ast::AstNodeId,
    name: &bstr::BStr,
) -> Option<Location> {
    let mut child_id = start_id;
    loop {
        let child = parsing.ast_node(child_id);
        let parent_node = child.parent()?;
        let parent_id = parent_node.id();
        let parent = parsing.ast_node(parent_id);
        match parent.payload() {
            AstNodePayload::Match => {
                // Children: [subject, arm_0, arm_1, ...] where each `case` arm contributes
                // (pattern, body) and each `else` arm contributes (body) only.
                let children = parent.children();
                for (pat_opt, body) in match_arm_children(&children) {
                    if body.id() == child_id {
                        if let Some(pat) = pat_opt {
                            if let AstNodePayload::PatCtor(_) = pat.payload() {
                                for var in pat.children() {
                                    if let Some(interned) = var.path() {
                                        if parsing.string(interned) == name {
                                            return Some(var.location());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        if parent.is_item() { return None; }
        child_id = parent_id;
    }
}

fn parse_word_value(s: &str) -> WordValue {
    let s = if let Some((val, _)) = s.split_once('w') { val } else { s };
    let s = s.replace('_', "");
    if let Some(hex) = s.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap_or(0)
    } else if let Some(bin) = s.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap_or(0)
    } else {
        s.parse().unwrap_or(0)
    }
}

/// The mono type of the expression at `loc`, substituted with the
/// owning generic mod def instance's type arguments (`env`).
fn mono_type(db: &Db, loc: &Location, env: &[Type]) -> Type {
    let typ = db.get_typeof(loc.clone()).unwrap_or(Type::Bit);
    if env.is_empty() {
        typ
    } else {
        substitute(&typ, env)
    }
}

fn convert_ast_expr(db: &Db, loc: Location, env: &[Type]) -> Arc<Expr> {
    let typ = mono_type(db, &loc, env);
    let parsing = db.get_parsing(loc.package());
    let node = parsing.ast_node(loc.ast_node_id());

    let payload = match node.payload() {
        AstNodePayload::ExprReference => {
            let mut name = parsing.string(node.path().unwrap()).to_owned();
            let needs_it_rewrite = name.starts_with(b"it.") || name == b"it";
            let mut it_target: Option<BString> = None;
            let mut current_id = loc.ast_node_id();
            loop {
                let current = parsing.ast_node(current_id);
                if current.is_item() { break; }
                if needs_it_rewrite && it_target.is_none() {
                    match current.payload() {
                        AstNodePayload::Submodule(m) =>
                            it_target = Some(parsing.string(m.name).to_owned()),
                        AstNodePayload::Component(c) =>
                            it_target = Some(parsing.string(c.name).to_owned()),
                        AstNodePayload::Socket(s) =>
                            it_target = Some(parsing.string(s.name).to_owned()),
                        _ => {}
                    }
                }
                current_id = current.parent()
                    .map(|p| p.id())
                    .expect("ExprReference not contained in any item");
            }
            if let Some(target) = it_target {
                if name == b"it" {
                    name = target;
                } else {
                    let suffix: BString = name[3..].to_owned().into();
                    name = target;
                    name.push(b'.');
                    name.extend_from_slice(&suffix);
                }
            }
            let item_node = parsing.ast_node(current_id);
            let item_name = parsing.string(item_node.name().unwrap()).to_owned();
            let symboltable = db.get_symboltable();
            let item_symbol = symboltable
                .resolve_item_in_package(item_name.as_bstr(), loc.package())
                .unwrap();
            let component_analysis = db.get_component_analysis(item_symbol.id());
            // Pattern-bound variables (introduced by enclosing match arms)
            // shadow components in the surrounding module scope, matching the
            // semantics used by the typechecker and Verilog backend.
            if let Some(bound_loc) = resolve_pattern_binding(&parsing, loc.ast_node_id(), name.as_bstr()) {
                ExprPayload::Reference(payload::Reference { referent: Referent::Location(bound_loc) })
            } else if let Some(component) = component_analysis.resolve(name.as_bstr()) {
                ExprPayload::Reference(payload::Reference { referent: Referent::Component(component.id()) })
            } else {
                todo!("ExprReference {:?} does not resolve to a component or a pattern-bound variable", name)
            }
        }
        AstNodePayload::ExprParen => {
            let child_loc = node.child(0).location();
            ExprPayload::Paren(payload::Paren { subject: convert_ast_expr(db, child_loc, env) })
        }
        AstNodePayload::When => {
            use crate::syntax::ast::when_arm_children;
            let children = node.children();
            let arms = when_arm_children(&children);
            let mut branches: Vec<(Arc<Expr>, Arc<Expr>)> = Vec::new();
            let mut else_branch_opt: Option<Arc<Expr>> = None;

            for (guard_opt, body) in arms {
                if let Some(guard) = guard_opt {
                    let guard_expr = convert_ast_expr(db, guard.location(), env);
                    let body_expr = convert_ast_expr(db, body.location(), env);
                    branches.push((guard_expr, body_expr));
                } else {
                    else_branch_opt = Some(convert_ast_expr(db, body.location(), env));
                }
            }

            let else_branch = else_branch_opt.expect("when must have else arm");
            ExprPayload::When(payload::When { branches, else_branch })
        }
        AstNodePayload::Match => {
            let children = node.children();
            let subject_loc = children[0].location();
            let subject_typ = mono_type(db, &subject_loc, env);
            let arm_data: Vec<(payload::Pat, Location)> = match_arm_children(&children)
                .into_iter()
                .map(|(pat_opt, body)| {
                    let pattern = match pat_opt {
                        Some(pat) => convert_pattern(db, &parsing, pat, &subject_typ),
                        None => payload::Pat::Else,
                    };
                    (pattern, body.location())
                })
                .collect();
            let subject = convert_ast_expr(db, subject_loc, env);
            let mut arms: Vec<(payload::Pat, Arc<Expr>)> = Vec::new();
            for (pat, body_loc) in arm_data {
                arms.push((pat, convert_ast_expr(db, body_loc, env)));
            }
            ExprPayload::Match(payload::Match { subject, arms })
        }
        AstNodePayload::ExprBitLit(lit) => {
            ExprPayload::BitLit(payload::BitLit { value: lit.literal })
        }
        AstNodePayload::ExprWordLit(lit) => {
            let s = parsing.string(lit.literal).to_str_lossy().into_owned();
            let width = if let Type::Word(w) = &typ { *w } else { unreachable!() };
            ExprPayload::WordLit(payload::WordLit { width, value: parse_word_value(&s) })
        }
        AstNodePayload::ExprStrLit(lit) => {
            let value = parsing.string(lit.literal).to_owned();
            ExprPayload::StrLit(payload::StrLit { value })
        }
        AstNodePayload::ExprBinOp(binop) => {
            let lhs_loc = node.child(0).location();
            let rhs_loc = node.child(1).location();
            let op = binop.op;
            let lhs = convert_ast_expr(db, lhs_loc, env);
            let rhs = convert_ast_expr(db, rhs_loc, env);
            ExprPayload::BinOp(payload::BinOp { lhs, op, rhs })
        }
        AstNodePayload::ExprUnOp(unop) => {
            let child_loc = node.child(0).location();
            let op = unop.op;
            let subject = convert_ast_expr(db, child_loc, env);
            ExprPayload::UnOp(payload::UnOp { op, subject })
        }
        AstNodePayload::ExprFn => {
            // Check if typing tagged this Fn as a primitive and dispatch accordingly
            let exprroot = db.get_exprroot_for(loc.clone());
            let typing = db.get_typing(exprroot);
            let tag = typing.tag(loc.clone());

            if let Some(primitive) = tag.primitive() {
                // Dispatch based on primitive type
                use crate::types::typing::Primitive;
                let arg_locs: Vec<Location> = node.args().unwrap().into_iter().map(|c| c.location()).collect();
                let mut args: Vec<Arc<Expr>> = Vec::new();
                for l in arg_locs { args.push(convert_ast_expr(db, l, env)); }

                match primitive {
                    Primitive::Word => ExprPayload::Word(payload::Word { args }),
                    Primitive::Zext => {
                        assert!(args.len() == 1, "zext expects exactly 1 argument");
                        ExprPayload::Zext(payload::Zext { subject: args[0].clone() })
                    }
                    Primitive::Sext => {
                        assert!(args.len() == 1, "sext expects exactly 1 argument");
                        ExprPayload::Sext(payload::Sext { subject: args[0].clone() })
                    }
                    Primitive::AsClock => {
                        assert!(args.len() == 1, "asclock expects exactly 1 argument");
                        ExprPayload::AsClock(payload::AsClock { subject: args[0].clone() })
                    }
                    Primitive::Cast => {
                        assert!(args.len() == 1, "cast expects exactly 1 argument");
                        ExprPayload::Cast(payload::Cast { subject: args[0].clone() })
                    }
                    Primitive::Trunc => {
                        assert!(args.len() == 1, "trunc expects exactly 1 argument");
                        ExprPayload::Trunc(payload::Trunc { subject: args[0].clone() })
                    }
                    Primitive::Any => {
                        assert!(args.len() == 1, "any expects exactly 1 argument");
                        ExprPayload::Any(payload::Any { subject: args[0].clone() })
                    }
                    Primitive::All => {
                        assert!(args.len() == 1, "all expects exactly 1 argument");
                        ExprPayload::All(payload::All { subject: args[0].clone() })
                    }
                    Primitive::Mux => {
                        assert!(args.len() == 3, "mux expects exactly 3 arguments");
                        // mux(cond, a, b) -> when { case cond => a; else => b }
                        let branches = vec![(args[0].clone(), args[1].clone())];
                        ExprPayload::When(payload::When {
                            branches,
                            else_branch: args[2].clone(),
                        })
                    }
                }
            } else {
                // Not a primitive, treat as regular function call
                let subject_loc = node.child(0).location();
                let arg_locs: Vec<Location> = node.args().unwrap().into_iter().map(|c| c.location()).collect();
                let subject = convert_ast_expr(db, subject_loc, env);
                let mut args: Vec<Arc<Expr>> = Vec::new();
                for l in arg_locs { args.push(convert_ast_expr(db, l, env)); }
                ExprPayload::Fn(payload::Fn { subject, args })
            }
        }
        AstNodePayload::ExprCtor(_expr_ctor) => {
            // All constructors — union ctors and enum payload
            // enumerants — resolve through the typing tag, which
            // records the ctor/enumerant symbol for the expected type.
            let arg_locs: Vec<Location> = node.children().iter().map(|c| c.location()).collect();
            let mut args: Vec<Arc<Expr>> = Vec::new();
            for l in arg_locs { args.push(convert_ast_expr(db, l, env)); }

            let exprroot = db.get_exprroot_for(loc.clone());
            let typing = db.get_typing(exprroot);
            let symbol_id = typing.tag(loc.clone()).symbol_id().unwrap();
            ExprPayload::Ctor(payload::Ctor { symbol_id, args })
        }
        AstNodePayload::ExprEnumerant(_) => {
            let exprroot = db.get_exprroot_for(loc.clone());
            let typing = db.get_typing(exprroot);
            let symbol_id = typing.tag(loc.clone()).symbol_id().unwrap();
            ExprPayload::Enumerant(payload::Enumerant { symbol_id })
        }
        AstNodePayload::ExprStruct => {
            let field_locs: Vec<Location> = node.children().iter().map(|c| c.location()).collect();
            let mut fields: Vec<(BString, Arc<Expr>)> = Vec::new();
            for (i, l) in field_locs.into_iter().enumerate() {
                fields.push((format!("{i}").into(), convert_ast_expr(db, l, env)));
            }
            ExprPayload::Struct(payload::Struct { fields })
        }
        AstNodePayload::ExprIndex(idx) => {
            let child_loc = node.child(0).location();
            let index = idx.index;
            let subject = convert_ast_expr(db, child_loc, env);
            ExprPayload::Index(payload::Index { subject, index })
        }
        AstNodePayload::ExprIndexDyn => {
            let subject_loc = node.child(0).location();
            let index_loc = node.child(1).location();
            let subject = convert_ast_expr(db, subject_loc, env);
            let index = convert_ast_expr(db, index_loc, env);
            ExprPayload::IndexDyn(payload::IndexDyn { subject, index })
        }
        AstNodePayload::ExprIndexRange(range) => {
            let child_loc = node.child(0).location();
            let (index_hi, index_lo) = (range.index_hi, range.index_lo);
            let subject = convert_ast_expr(db, child_loc, env);
            ExprPayload::IndexRange(payload::IndexRange { subject, index_hi, index_lo })
        }
        AstNodePayload::ExprAs => {
            let subject = convert_ast_expr(db, node.child(0).location(), env);
            ExprPayload::As(payload::As { subject })
        }
        AstNodePayload::ExprHole => ExprPayload::Hole(payload::Hole {}),
        AstNodePayload::ExprDontcare => ExprPayload::Dontcare(payload::Dontcare {}),
        other => unreachable!("expected expression node, got {:?}", other.kind()),
    };

    Arc::new(Expr { location: loc, typ, payload })
}
