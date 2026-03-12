use crate::common;
use crate::virir::{TypeId, Width};
use crate::source::Region;

#[derive(Debug)]
pub enum Expr {
    Reference(Reference),
    Literal(BitLit),
    BinOp(BinOp),
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
    pub value: bool,
    pub width: Width,
}

#[derive(Debug)]
pub struct BinOp {
    pub region: Region,
    pub typ: TypeId,
    pub op: common::BinOp,
}

impl BitLit {
    pub fn value(&self) -> bool {
        self.value
    }
}
