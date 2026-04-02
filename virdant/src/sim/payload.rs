use std::sync::Arc;

use bstr::BString;

use crate::common::{self, Width};
use crate::analysis::symbols::SymbolId;
use crate::sim::expr::{Expr, Referent};

#[derive(Debug)]
pub(super) struct Reference {
    pub(super) referent: Referent,
}

#[derive(Debug)]
pub(super) struct Paren {
    pub(super) subject: Arc<Expr>,
}

#[derive(Debug)]
pub(super) struct If {
    pub(super) branches: Vec<(Arc<Expr>, Arc<Expr>)>,
    pub(super) else_branch: Arc<Expr>,
}

#[derive(Debug)]
pub(super) enum MatchPattern {
    Ctor { symbol_id: SymbolId, bound_vars: Vec<BString> },
    Else,
}

#[derive(Debug)]
pub(super) struct Match {
    pub(super) subject: Arc<Expr>,
    pub(super) arms: Vec<(MatchPattern, Arc<Expr>)>,
}

#[derive(Debug)]
pub(super) struct BitLit {
    pub(super) value: bool,
}

#[derive(Debug)]
pub(super) struct WordLit {
    pub(super) width: Width,
    pub(super) value: u64, // TODO make this a common Value
}

#[derive(Debug)]
pub(super) struct StrLit {
    pub(super) value: BString,
}

#[derive(Debug)]
pub(super) struct BinOp {
    pub(super) lhs: Arc<Expr>,
    pub(super) op: common::BinOp,
    pub(super) rhs: Arc<Expr>,
}

#[derive(Debug)]
pub(super) struct UnOp {
    pub(super) op: common::UnOp,
    pub(super) subject: Arc<Expr>,
}

#[derive(Debug)]
pub(super) struct Method {
    pub(super) subject: Arc<Expr>,
    pub(super) method: BString,
    pub(super) args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub(super) struct Fn {
    pub(super) subject: Arc<Expr>,
    pub(super) args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub(super) struct Ctor {
    pub(super) symbol_id: SymbolId,
    pub(super) args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub(super) struct Enumerant {
    pub(super) symbol_id: SymbolId,
}

#[derive(Debug)]
pub(super) struct Struct {
    pub(super) fields: Vec<(BString, Arc<Expr>)>,
}

#[derive(Debug)]
pub(super) struct Index {
    pub(super) subject: Arc<Expr>,
    pub(super) index: u16,
}

#[derive(Debug)]
pub(super) struct IndexRange {
    pub(super) subject: Arc<Expr>,
    pub(super) index_hi: u16,
    pub(super) index_lo: u16,
}

#[derive(Debug)]
pub(super) struct Word {
    pub(super) args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub(super) struct Zext {
    pub(super) subject: Arc<Expr>,
}

#[derive(Debug)]
pub(super) struct Sext {
    pub(super) subject: Arc<Expr>,
}

#[derive(Debug)]
pub(super) struct As {
    pub(super) subject: Arc<Expr>,
}

#[derive(Debug)]
pub(super) struct Hole {
}
