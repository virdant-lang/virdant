use super::{Expr, Stmt};
use crate::common::Radix;

pub struct AssignBlocking {
    pub name: String,
    pub expr: Expr,
}

pub struct AssignNonBlocking {
    pub name: String,
    pub expr: Expr,
}

pub struct Display {
    pub exprs: Vec<Expr>,
}

pub struct Assert {
    pub exprs: Vec<Expr>,
}

pub struct Case {
    pub subject: Expr,
    pub items: Vec<CaseItem>,
}

pub struct CaseZ {
    pub subject: Expr,
    pub items: Vec<CaseItem>,
}

pub struct CaseItem {
    pub pattern: CasePattern,
    pub stmts: Vec<Stmt>,
}

pub enum CasePattern {
    Expr(Expr),
    PatternLit(PatternLit),
    Default,
}

pub struct PatternLit {
    pub width: u16,
    pub radix: Radix,
    pub pattern: String,
}
