use super::{Expr, Stmt};
use crate::common::Radix;

#[derive(Debug)]
pub struct AssignBlocking {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct AssignNonBlocking {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Display {
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Assert {
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Case {
    pub subject: Expr,
    pub items: Vec<CaseItem>,
}

#[derive(Debug)]
pub struct CaseZ {
    pub subject: Expr,
    pub items: Vec<CaseItem>,
}

#[derive(Debug)]
pub struct CaseItem {
    pub pattern: CasePattern,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum CasePattern {
    Expr(Expr),
    PatternLit(PatternLit),
    Default,
}

#[derive(Debug)]
pub struct PatternLit {
    pub width: u16,
    pub radix: Radix,
    pub pattern: String,
}
