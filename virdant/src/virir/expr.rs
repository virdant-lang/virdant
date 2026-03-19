use crate::common;
use crate::virir::TypeId;
use crate::source::Region;
use std::sync::Arc;

#[derive(Debug)]
pub enum Expr {
    Reference(Reference),
    BitLit(BitLit),
    WordLit(WordLit),
    BinOp(BinOp),
    UnOp(UnOp),
    If(If),
    Index(Index),
    IndexRange(IndexRange),
}

#[derive(Debug)]
pub struct Reference {
    pub region: Region,
    pub typ: TypeId,
    pub path: String,
}

#[derive(Debug)]
pub struct BitLit {
    pub region: Region,
    pub typ: TypeId,
    value: bool,
}

#[derive(Debug)]
pub struct WordLit {
    pub region: Region,
    pub typ: TypeId,
    pub value: u64,
}

#[derive(Debug)]
pub struct BinOp {
    pub region: Region,
    pub typ: TypeId,
    pub op: common::BinOp,
    pub lhs: Arc<Expr>,
    pub rhs: Arc<Expr>,
}

#[derive(Debug)]
pub struct UnOp {
    pub region: Region,
    pub typ: TypeId,
    pub op: common::UnOp,
    pub expr: Arc<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub region: Region,
    pub typ: TypeId,
    pub cond: Arc<Expr>,
    pub then_expr: Arc<Expr>,
    pub else_expr: Arc<Expr>,
}

#[derive(Debug)]
pub struct Index {
    pub region: Region,
    pub typ: TypeId,
    pub subject: Arc<Expr>,
    pub index: common::Width,
}

#[derive(Debug)]
pub struct IndexRange {
    pub region: Region,
    pub typ: TypeId,
    pub subject: Arc<Expr>,
    pub index_hi: common::Width,
    pub index_lo: common::Width,
}

impl BitLit {
    pub fn value(&self) -> bool {
        self.value
    }
}
