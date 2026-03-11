use crate::common;
use crate::virir::{TypeId, Width};
use crate::source::Region;

pub enum Expr {
    Reference(Reference),
    Literal(BitLit),
    BinOp(BinOp),
}

pub struct Reference {
    pub region: Region,
    pub typ: TypeId,
    pub path: String,
}

pub struct BitLit {
    pub region: Region,
    pub typ: TypeId,
    value: bool,
}

pub struct WordLit {
    pub region: Region,
    pub typ: TypeId,
    pub value: bool,
    pub width: Width,
}

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
