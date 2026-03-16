use super::*;

#[derive(Debug)]
pub struct Reference {
    pub name: String,
}

#[derive(Debug)]
pub struct BinOp {
    pub op: super::BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct UnOp {
    pub op: super::UnOp,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct BitLit {
    pub value: bool,
}

#[derive(Debug)]
pub struct WordLit {
    pub value: u128,
    pub width: Width,
    pub radix: Radix,
}

#[derive(Debug)]
pub struct StrLit {
    pub value: String,
}

#[derive(Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Concat {
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Repeat {
    pub count: Box<Expr>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Index {
    pub subject: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug)]
pub struct IndexRange {
    pub subject: Box<Expr>,
    pub index_hi: Box<Expr>,
    pub index_lo: Box<Expr>,
}
