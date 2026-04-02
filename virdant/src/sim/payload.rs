use std::sync::Arc;

use bstr::BString;

use crate::common;
use crate::analysis::symbols::SymbolId;
use crate::sim::expr::{Expr, Referent};

pub(super) struct Reference {
    pub(super) referent: Referent,
}

pub(super) struct Paren {
    pub(super) subject: Arc<Expr>,
}

pub(super) struct If {
    pub(super) branches: Vec<(Arc<Expr>, Arc<Expr>)>,
    pub(super) else_branch: Arc<Expr>,
}

pub(super) enum MatchPattern {
    Ctor { symbol_id: SymbolId, bound_vars: Vec<BString> },
    Else,
}

pub(super) struct Match {
    pub(super) subject: Arc<Expr>,
    pub(super) arms: Vec<(MatchPattern, Arc<Expr>)>,
}

pub(super) struct BitLit {
    pub(super) value: bool,
}

pub(super) struct WordLit {
    pub(super) value: u64, // TODO make this a common Value
}

pub(super) struct StrLit {
    pub(super) value: BString,
}

pub(super) struct BinOp {
    pub(super) lhs: Arc<Expr>,
    pub(super) op: common::BinOp,
    pub(super) rhs: Arc<Expr>,
}

pub(super) struct UnOp {
    pub(super) op: common::UnOp,
    pub(super) subject: Arc<Expr>,
}

pub(super) struct Method {
    pub(super) subject: Arc<Expr>,
    pub(super) method: BString,
    pub(super) args: Vec<Arc<Expr>>,
}

pub(super) struct Fn {
    pub(super) subject: Arc<Expr>,
    pub(super) args: Vec<Arc<Expr>>,
}

pub(super) struct Ctor {
    pub(super) symbol_id: SymbolId,
    pub(super) args: Vec<Arc<Expr>>,
}

pub(super) struct Enumerant {
    pub(super) symbol_id: SymbolId,
}

pub(super) struct Struct {
    pub(super) fields: Vec<(BString, Arc<Expr>)>,
}

pub(super) struct Index {
    pub(super) subject: Arc<Expr>,
    pub(super) index: u16,
}

pub(super) struct IndexRange {
    pub(super) subject: Arc<Expr>,
    pub(super) index_hi: u16,
    pub(super) index_lo: u16,
}

pub(super) struct Word {
    pub(super) args: Vec<Arc<Expr>>,
}

pub(super) struct Zext {
    pub(super) subject: Arc<Expr>,
}

pub(super) struct Sext {
    pub(super) subject: Arc<Expr>,
}

pub(super) struct As {
    pub(super) subject: Arc<Expr>,
}

pub(super) struct Hole {
}
