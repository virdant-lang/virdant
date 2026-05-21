use std::sync::Arc;

use bstr::BString;

use crate::analysis::Location;
use crate::common::{self, Width, WordValue};
use crate::analysis::symbols::SymbolId;
use crate::sim::expr::{Expr, Referent};

#[derive(Debug)]
pub struct Reference {
    pub referent: Referent,
}

#[derive(Debug)]
pub struct Paren {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub branches: Vec<(Arc<Expr>, Arc<Expr>)>,
    pub else_branch: Arc<Expr>,
}

#[derive(Debug)]
pub enum Pat {
    Ctor { symbol_id: SymbolId, bound_vars: Vec<(BString, Location)> },
    WordLit { width: Width, value: WordValue },
    BitLit { value: bool },
    Else,
}

#[derive(Debug)]
pub struct Match {
    pub subject: Arc<Expr>,
    pub arms: Vec<(Pat, Arc<Expr>)>,
}

#[derive(Debug)]
pub struct BitLit {
    pub value: bool,
}

#[derive(Debug)]
pub struct WordLit {
    pub width: Width,
    pub value: WordValue,
}

#[derive(Debug)]
pub struct StrLit {
    #[allow(dead_code)]
    pub value: BString,
}

#[derive(Debug)]
pub struct BinOp {
    pub lhs: Arc<Expr>,
    pub op: common::BinOp,
    pub rhs: Arc<Expr>,
}

#[derive(Debug)]
pub struct UnOp {
    pub op: common::UnOp,
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct Method {
    pub subject: Arc<Expr>,
    #[allow(dead_code)]
    pub method: BString,
    pub args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub struct Fn {
    pub subject: Arc<Expr>,
    pub args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub struct Ctor {
    pub symbol_id: SymbolId,
    pub args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub struct Enumerant {
    pub symbol_id: SymbolId,
}

#[derive(Debug)]
pub struct Struct {
    pub fields: Vec<(BString, Arc<Expr>)>,
}

#[derive(Debug)]
pub struct Index {
    pub subject: Arc<Expr>,
    pub index: u16,
}

#[derive(Debug)]
pub struct IndexRange {
    pub subject: Arc<Expr>,
    pub index_hi: u16,
    pub index_lo: u16,
}

#[derive(Debug)]
pub struct Word {
    pub args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub struct Zext {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct Sext {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct Cast {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct Trunc {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct Any {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct All {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct As {
    pub subject: Arc<Expr>,
}

#[derive(Debug)]
pub struct Hole {
}

#[derive(Debug)]
pub struct Dontcare {
}
