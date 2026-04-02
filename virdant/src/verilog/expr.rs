use super::*;

#[derive(Debug, Clone)]
pub struct Reference {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub op: super::BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub op: super::UnOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BitLit {
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct WordLit {
    pub value: u128,
    pub width: Width,
    pub radix: Radix,
}

#[derive(Debug, Clone)]
pub struct StrLit {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Concat {
    pub exprs: Vec<Expr>,
    /// Total bit width of this concatenation. Set to 0 when unknown.
    pub width: super::Width,
}

#[derive(Debug, Clone)]
pub struct Repeat {
    pub count: Box<Expr>,
    pub exprs: Vec<Expr>,
    /// Total bit width of the replicated result. Set to 0 when unknown.
    pub width: super::Width,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub subject: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct IndexRange {
    pub subject: Box<Expr>,
    pub index_hi: Box<Expr>,
    pub index_lo: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct XLit {
    pub width: Width,
}
