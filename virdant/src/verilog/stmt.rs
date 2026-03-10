use super::Expr;

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
