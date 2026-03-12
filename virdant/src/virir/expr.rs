use crate::common;
use crate::virir::{TypeId, Width};
use crate::source::Region;
use std::sync::Arc;

#[derive(Debug)]
pub enum Expr {
    Reference(Reference),
    BitLit(BitLit),
    WordLit(WordLit),
    BinOp(BinOp),
    If(If),
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
pub struct If {
    pub region: Region,
    pub typ: TypeId,
    pub cond: Arc<Expr>,
    pub then_expr: Arc<Expr>,
    pub else_expr: Arc<Expr>,
}

impl BitLit {
    pub fn value(&self) -> bool {
        self.value
    }
}
