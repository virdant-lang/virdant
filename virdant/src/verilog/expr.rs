use super::*;

pub struct Reference {
    pub name: String,
}

pub struct BinOp {
    pub op: super::BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct UnOp {
    pub op: super::UnOp,
    pub expr: Box<Expr>,
}

pub struct BitLit {
    pub value: bool,
}

pub struct WordLit {
    pub value: u128,
    pub width: u16,
    pub radix: Radix,
}

pub struct StrLit {
    pub value: String,
}

pub struct If {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

pub struct Index {
    pub subject: Box<Expr>,
    pub index: Box<Expr>,
}

pub struct IndexRange {
    pub subject: Box<Expr>,
    pub index_hi: Box<Expr>,
    pub index_lo: Box<Expr>,
}
