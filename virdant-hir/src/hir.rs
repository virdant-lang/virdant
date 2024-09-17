use std::sync::Arc;

use virdant_common::WordVal;
use virdant_tokenizer::Token;
use virdant_parser::{Ident, StaticIndex};

use crate::*;

#[derive(Debug)]
pub struct Hir {
    st: SymbolTable,
}

#[derive(Debug)]
pub enum Expr {
    Reference(Referent),
    BitLit(Token, bool),
    WordLit(Token, WordVal),
    Word(Vec<Arc<Expr>>),
    Bit(bool),
    UnOp(UnOp, Arc<Expr>),
    BinOp(Arc<Expr>, BinOp, Arc<Expr>),
    MethodCall(Arc<Expr>, Ident, Vec<Arc<Expr>>),
    Struct(TypeDef, Vec<Assign>),
    FnCall(Id<FnDef>, Vec<Arc<Expr>>),
    Field(Arc<Expr>, Id<Field>),
    Ctor(Id<Ctor>, Vec<Arc<Expr>>),
    Enumerant(Id<Enumerant>),
    As(Arc<Expr>, Arc<Type>),
    Idx(Arc<Expr>, StaticIndex),
    IdxRange(Arc<Expr>, StaticIndex, StaticIndex),
    Cat(Vec<Arc<Expr>>),
    Zext(Arc<Expr>),
    Sext(Arc<Expr>),
    If { subject: Arc<Expr>, true_branch: Arc<Expr>, false_branch: Arc<Expr> },
    Match { subject: Arc<Expr>, arms: Vec<MatchArm> },
}

#[derive(Debug)]
pub struct Assign {
    pub field: Id<Field>,
    pub value: Arc<Expr>,
}

#[derive(Debug)]
pub enum Pat {
    Bind(Token),
    At(Token, Option<Vec<Arc<Pat>>>),
    Else,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
}

#[derive(Debug)]
pub enum Referent {
//    Binding(Id<Binding>),
    Component(Id<Component>),
}


#[derive(Debug)]
pub struct MatchArm { 
    pub pat: Pat,
    pub expr: Arc<Expr>,
}

#[derive(Debug)]
pub struct Type {
//    pub generics: Vec<Id<Generic>>,
    pub typedef: Id<TypeDef>,
    pub typeargs: Option<Vec<TypeArg>>,
}

#[derive(Debug)]
pub enum TypeArg {
    Nat(WordVal),
    Type(Arc<Type>),
}
