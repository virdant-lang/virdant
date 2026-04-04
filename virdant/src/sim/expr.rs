use std::sync::Arc;
use bstr::{BString, ByteSlice};

use crate::analysis::component::ComponentId;
use crate::analysis::drivers::{Driver, DriverIf};
use crate::db::{Builder, Db};
use crate::sim::payload;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;
use crate::analysis::Location;

#[derive(Debug)]
pub(super) struct Expr {
    location: Location,
    typ: Type,
    payload: ExprPayload,
}

#[derive(Debug)]
pub(super) enum ExprPayload {
    Reference(payload::Reference),
    Paren(payload::Paren),
    If(payload::If),
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
    IndexRange(payload::IndexRange),
    Word(payload::Word),
    Zext(payload::Zext),
    Sext(payload::Sext),
    As(payload::As),
    Hole(payload::Hole),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Referent {
    Component(ComponentId),
}

pub fn driver_to_expr(db: &Db, driver: &Driver) -> Arc<Expr> {
    match driver {
        Driver::Expr(_, loc) => convert_ast_expr(db, loc.clone()),
        Driver::If(driver_if) => convert_driver_if(db, driver_if),
    }
}

fn convert_driver_if(db: &Db, driver_if: &DriverIf) -> Arc<Expr> {
    let location = driver_if.clauses[0].0.clone();

    let mut branches: Vec<(Arc<Expr>, Arc<Expr>)> = Vec::new();
    for (cond_loc, sub_driver) in &driver_if.clauses {
        let cond = convert_ast_expr(db, cond_loc.clone());
        let body = driver_to_expr(db, sub_driver.as_ref());
        branches.push((cond, body));
    }

    let else_branch = match &driver_if.else_clause {
        Some(else_driver) => driver_to_expr(db, else_driver.as_ref()),
        None => {
            let typ = Type::Bit;
            Arc::new(Expr { location: location.clone(), typ, payload: ExprPayload::Hole(payload::Hole {}) })
        }
    };

    let typ = else_branch.typ.clone();
    Arc::new(Expr { location, typ, payload: ExprPayload::If(payload::If { branches, else_branch }) })
}

// Recursively build `If` branches from the flat [cond, then, cond, then, ..., else] child list.
fn build_if_branches(db: &Db, locs: &[Location]) -> (Vec<(Arc<Expr>, Arc<Expr>)>, Arc<Expr>) {
    let cond = convert_ast_expr(db, locs[0].clone());
    let then_expr = convert_ast_expr(db, locs[1].clone());
    if locs.len() == 3 {
        let else_expr = convert_ast_expr(db, locs[2].clone());
        (vec![(cond, then_expr)], else_expr)
    } else {
        let (mut rest, else_expr) = build_if_branches(db, &locs[2..]);
        rest.insert(0, (cond, then_expr));
        (rest, else_expr)
    }
}

fn parse_word_value(s: &str) -> u64 {
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

fn convert_ast_expr(db: &Db, loc: Location) -> Arc<Expr> {
    let typ = db.get_typeof(loc.clone()).unwrap_or(Type::Bit);
    let parsing = db.get_parsing(loc.package());
    let node = parsing.ast_node(loc.ast_node_id());

    let payload = match node.payload() {
        AstNodePayload::ExprReference => {
            let name = parsing.string(node.path().unwrap()).to_owned();
            let mut current_id = loc.ast_node_id();
            loop {
                let current = parsing.ast_node(current_id);
                if current.is_item() { break; }
                current_id = current.parent()
                    .map(|p| p.id())
                    .expect("ExprReference not contained in any item");
            }
            let item_node = parsing.ast_node(current_id);
            let item_name = parsing.string(item_node.name().unwrap()).to_owned();
            let symboltable = db.get_symboltable();
            let item_symbol = symboltable
                .resolve_item_in_package(item_name.as_bstr(), loc.package())
                .unwrap();
            let component_analysis = db.get_component_analysis(item_symbol.id());
            if let Some(component) = component_analysis.resolve(name.as_bstr()) {
                ExprPayload::Reference(payload::Reference { referent: Referent::Component(component.id()) })
            } else {
                todo!("ExprReference {:?} does not resolve to a component — it may be a pattern-bound variable introduced by a match arm, which is not yet supported", name)
            }
        }
        AstNodePayload::ExprParen => {
            let child_loc = node.child(0).location();
            ExprPayload::Paren(payload::Paren { subject: convert_ast_expr(db, child_loc) })
        }
        AstNodePayload::ExprIf => {
            let child_locs: Vec<Location> = node.children().iter().map(|c| c.location()).collect();
            let (branches, else_branch) = build_if_branches(db, &child_locs);
            ExprPayload::If(payload::If { branches, else_branch })
        }
        AstNodePayload::ExprMatch => {
            let children = node.children();
            let subject_loc = children[0].location();
            let num_arms = (children.len() - 1) / 2;
            let exprroot = db.get_exprroot_for(loc.clone());
            let typing = db.get_typing(exprroot);
            // Collect all arm data as owned values before making recursive db calls.
            let arm_data: Vec<(payload::MatchPattern, Location)> = (0..num_arms).map(|i| {
                let pat = &children[2 * i + 1];
                let body_loc = children[2 * i + 2].location();
                let pattern = match pat.payload() {
                    AstNodePayload::PatEnumerant(_) => {
                        let symbol_id = typing.tag(pat.location()).symbol_id().unwrap();
                        let bound_vars: Vec<BString> = pat.children().iter()
                            .filter_map(|c| match c.payload() {
                                AstNodePayload::PatIdent(ident) =>
                                    Some(parsing.string(ident.name).to_owned()),
                                _ => None,
                            })
                            .collect();
                        payload::MatchPattern::Ctor { symbol_id, bound_vars }
                    }
                    _ => payload::MatchPattern::Else,
                };
                (pattern, body_loc)
            }).collect();
            let subject = convert_ast_expr(db, subject_loc);
            let mut arms: Vec<(payload::MatchPattern, Arc<Expr>)> = Vec::new();
            for (pat, body_loc) in arm_data {
                arms.push((pat, convert_ast_expr(db, body_loc)));
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
            let lhs = convert_ast_expr(db, lhs_loc);
            let rhs = convert_ast_expr(db, rhs_loc);
            ExprPayload::BinOp(payload::BinOp { lhs, op, rhs })
        }
        AstNodePayload::ExprUnOp(unop) => {
            let child_loc = node.child(0).location();
            let op = unop.op;
            let subject = convert_ast_expr(db, child_loc);
            ExprPayload::UnOp(payload::UnOp { op, subject })
        }
        AstNodePayload::ExprMethod(method_payload) => {
            let subject_loc = node.child(0).location();
            let method = parsing.string(method_payload.method).to_owned();
            let arg_locs: Vec<Location> = node.children().iter().skip(1).map(|c| c.location()).collect();
            let subject = convert_ast_expr(db, subject_loc);
            let mut args: Vec<Arc<Expr>> = Vec::new();
            for l in arg_locs { args.push(convert_ast_expr(db, l)); }
            ExprPayload::Method(payload::Method { subject, method, args })
        }
        AstNodePayload::ExprFn => {
            let subject_loc = node.child(0).location();
            let arg_locs: Vec<Location> = node.children().iter().skip(1).map(|c| c.location()).collect();
            let subject = convert_ast_expr(db, subject_loc);
            let mut args: Vec<Arc<Expr>> = Vec::new();
            for l in arg_locs { args.push(convert_ast_expr(db, l)); }
            ExprPayload::Fn(payload::Fn { subject, args })
        }
        AstNodePayload::ExprCtor(_) => {
            let exprroot = db.get_exprroot_for(loc.clone());
            let typing = db.get_typing(exprroot);
            let symbol_id = typing.tag(loc.clone()).symbol_id().unwrap();
            let arg_locs: Vec<Location> = node.children().iter().map(|c| c.location()).collect();
            let mut args: Vec<Arc<Expr>> = Vec::new();
            for l in arg_locs { args.push(convert_ast_expr(db, l)); }
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
                fields.push((format!("{i}").into(), convert_ast_expr(db, l)));
            }
            ExprPayload::Struct(payload::Struct { fields })
        }
        AstNodePayload::ExprIndex(idx) => {
            let child_loc = node.child(0).location();
            let index = idx.index;
            let subject = convert_ast_expr(db, child_loc);
            ExprPayload::Index(payload::Index { subject, index })
        }
        AstNodePayload::ExprIndexRange(range) => {
            let child_loc = node.child(0).location();
            let (index_hi, index_lo) = (range.index_hi, range.index_lo);
            let subject = convert_ast_expr(db, child_loc);
            ExprPayload::IndexRange(payload::IndexRange { subject, index_hi, index_lo })
        }
        AstNodePayload::ExprWord => {
            let arg_locs: Vec<Location> = node.children().iter().map(|c| c.location()).collect();
            let mut args: Vec<Arc<Expr>> = Vec::new();
            for l in arg_locs { args.push(convert_ast_expr(db, l)); }
            ExprPayload::Word(payload::Word { args })
        }
        AstNodePayload::ExprZext => {
            let subject = convert_ast_expr(db, node.child(0).location());
            ExprPayload::Zext(payload::Zext { subject })
        }
        AstNodePayload::ExprSext => {
            let subject = convert_ast_expr(db, node.child(0).location());
            ExprPayload::Sext(payload::Sext { subject })
        }
        AstNodePayload::ExprAs => {
            let subject = convert_ast_expr(db, node.child(0).location());
            ExprPayload::As(payload::As { subject })
        }
        AstNodePayload::ExprHole => ExprPayload::Hole(payload::Hole {}),
        other => unreachable!("expected expression node, got {:?}", other.kind()),
    };

    Arc::new(Expr { location: loc, typ, payload })
}
