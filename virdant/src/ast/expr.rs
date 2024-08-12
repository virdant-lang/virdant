use crate::common::*;
use crate::ast::Ast;
use crate::id::*;
use crate::types::Type;

use std::sync::Arc;

use internment::Intern;

pub type Ident = Intern<String>;
pub type Path = Vec<Ident>;
pub type QualIdent = Intern<String>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Reference(Path),
    Word(WordLit),
    Bit(bool),
    MethodCall(Arc<Expr>, Ident, Vec<Arc<Expr>>),
    Field(Arc<Expr>, Ident),
    Struct(QualIdent, Vec<(Ident, Arc<Expr>)>),
    Ctor(Ident, Vec<Arc<Expr>>),
    Idx(Arc<Expr>, StaticIndex),
    IdxRange(Arc<Expr>, StaticIndex, StaticIndex),
    Cat(Vec<Arc<Expr>>),
    If(Arc<Expr>, Arc<Expr>, Arc<Expr>),
    Match(Arc<Expr>, Option<Ast>, Vec<MatchArm>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WordLit {
    pub value: WordVal,
    pub width: Option<Width>,
    pub spelling: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Driver(pub Path, pub DriverType, pub Arc<Expr>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm(pub Pat, pub Arc<Expr>);

impl MatchArm {
    pub fn pat(&self) -> &Pat {
        &self.0
    }

    pub fn expr(&self) -> Arc<Expr> {
        self.1.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    CtorAt(Ident, Vec<Pat>),
    Bind(Ident),
    Else,
}

impl Expr {
    pub fn from_ast(expr_ast: Ast) -> Arc<Expr> {
        assert!(expr_ast.is_expr());
        let child = expr_ast.child(0);

        if child.is_expr_if() {
            Expr::ast_to_expr_if(child)
        } else if child.is_expr_match() {
            Expr::ast_to_expr_match(child)
        } else if child.is_expr_call() {
            Expr::ast_to_expr_call(child)
        } else {
            unreachable!()
        }
    }

    fn ast_to_expr_if(expr_if_ast: Ast) -> Arc<Expr> {
        let subject_ast = expr_if_ast.subject().unwrap();
        let subject = Expr::from_ast(subject_ast);
        let true_expr = Expr::from_ast(expr_if_ast.child(2));

        let children = &mut expr_if_ast.children().skip(3);

        let mut else_ifs: Vec<(Arc<Expr>, Arc<Expr>)> = vec![];
        let mut else_expr: Option<Arc<Expr>> = None;

        while let Some(_child) = children.next() {
            let node = children.next().unwrap();
            // if it's an "if" as in "else if"
            if node.is_kw_if() {
                let elseif_subject = Expr::from_ast(children.next().unwrap());
                let elseif_expr = Expr::from_ast(children.next().unwrap());
                else_ifs.push((elseif_subject, elseif_expr));
            } else {
                else_expr = Some(Expr::from_ast(node));
            }
        }

        let mut result = else_expr.unwrap();

        for (cond, expr) in else_ifs {
            result = Arc::new(Expr::If(cond, expr, result));
        }

        Arc::new(Expr::If(subject, true_expr, result))
    }

    fn ast_to_expr_match(expr_match_ast: Ast) -> Arc<Expr> {
        let subject_ast = expr_match_ast.subject().unwrap();
        let subject = Expr::from_ast(subject_ast);

        let ascription_ast = expr_match_ast.typ();

        let mut match_arms = vec![];
        for node in expr_match_ast.children() {
            if node.is_match_arm() {
                match_arms.push(Expr::ast_to_expr_match_arm(node));
            }
        }

        Arc::new(Expr::Match(subject, ascription_ast, match_arms))
    }

    fn ast_to_expr_match_arm(match_arm_ast: Ast) -> MatchArm {
        let pat_ast = match_arm_ast.pat().unwrap();
        let expr_ast = match_arm_ast.expr().unwrap();
        let pat = Expr::ast_to_pat(pat_ast);
        let expr = Expr::from_ast(expr_ast);
        MatchArm(pat, expr)
    }

    fn ast_to_pat(pat_ast: Ast) -> Pat {
        let first_node = pat_ast.child(0);
        if first_node.is_ctor() {
            let mut subpats = vec![];
            for arg in pat_ast.child(1).children() {
                subpats.push(Expr::ast_to_pat(arg));
            }
            Pat::CtorAt(Ident::new(first_node.as_str()[1..].to_string()), subpats)
        } else if first_node.is_ident() {
            Pat::Bind(Ident::new(first_node.as_str().to_string()))
        } else if first_node.is_kw_else() {
            Pat::Bind(Ident::new(first_node.as_str().to_string()))
        } else {
            unreachable!()
        }
    }

    fn ast_to_expr_call(expr_call_ast: Ast) -> Arc<Expr> {
        let expr_base_ast = expr_call_ast.child(0);
        let mut result = Expr::ast_to_expr_base(expr_base_ast);

        for expr_call_suffix_ast in expr_call_ast.children().skip(1) {
            if let Some(method) = expr_call_suffix_ast.method() {
                let args = expr_call_suffix_ast.args().unwrap();
                let mut arg_exprs = vec![];
                for arg in args {
                    arg_exprs.push(Expr::from_ast(arg));
                }

                result = Arc::new(Expr::MethodCall(result, method.to_string().into(), arg_exprs));
            } else if let Some(field) = expr_call_suffix_ast.field() {
                result = Arc::new(Expr::Field(result, field.to_string().into()));
            } else if let Some(j) = expr_call_suffix_ast.j() {
                let i = expr_call_suffix_ast.i().unwrap();
                result = Arc::new(Expr::IdxRange(result, j, i));
            } else if let Some(i) = expr_call_suffix_ast.i() {
                result = Arc::new(Expr::Idx(result, i));
            } else {
                unreachable!()
            }
        }

        result
    }

    fn ast_to_expr_base(expr_base_ast: Ast) -> Arc<Expr> {
        let child = expr_base_ast.child(0);

        let expr = if child.is_wordlit() {
            let spelling = child.as_str();
            Expr::Word(parse_wordlit(&spelling))
        } else if child.is_bitlit() {
            let spelling = child.as_str();
            Expr::Bit(parse_bitlit(&spelling))
        } else if child.is_path() {
            let path = child.as_str()
                .split('.')
                .map(|s| Intern::new(s.to_string()))
                .collect::<Vec<_>>();
            Expr::Reference(path)
        } else if child.is_ctor() {
            let mut args = vec![];
            for arg in expr_base_ast.args().unwrap() {
                args.push(Expr::from_ast(arg));
            }
            Expr::Ctor(Ident::new(child.as_str()[1..].to_string()), args)
        } else if child.is_kw_cat() {
            let mut args = vec![];
            for arg in expr_base_ast.args().unwrap() {
                args.push(Expr::from_ast(arg));
            }
            Expr::Cat(args)
        } else if child.is_struct() {
            let struct_name = child.as_str();
            let mut assigns = vec![];
            for assign in expr_base_ast.assigns().unwrap() {
                let field = Intern::new(assign.get_as_str("field").unwrap().to_string());
                let expr = Expr::from_ast(assign.expr().unwrap());
                assigns.push((field, expr));
            }
            Expr::Struct(Intern::new(struct_name[1..].to_string()), assigns)
        } else {
            unreachable!()
        };

        Arc::new(expr)
    }
}

fn parse_wordlit(wordlit: &str) -> WordLit {
    let spelling = wordlit.to_string();

    let (value, width) = if let Some(idx) = wordlit.find("w") {
        let value = str::parse(&spelling[..idx]).unwrap();
        let width = str::parse(&spelling[idx+1..]).unwrap();
        (value, Some(width))
    } else {
        (str::parse(&spelling).unwrap(), None)
    };

    WordLit {
        value,
        width,
        spelling,
    }
}

fn parse_bitlit(wordlit: &str) -> bool {
    str::parse(wordlit).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedExpr {
    Reference(Type, Referent),
    Word(Type, WordLit),
    Bit(Type, bool),
    MethodCall(Type, Arc<TypedExpr>, Ident, Vec<Arc<TypedExpr>>),
    Struct(Type, QualIdent, Vec<(Ident, Arc<TypedExpr>)>),
    Field(Type, Arc<TypedExpr>, Ident),
    Ctor(Type, Ident, Vec<Arc<TypedExpr>>),
    Idx(Type, Arc<TypedExpr>, StaticIndex),
    IdxRange(Type, Arc<TypedExpr>, StaticIndex, StaticIndex),
    Cat(Type, Vec<Arc<TypedExpr>>),
    If(Type, Arc<TypedExpr>, Arc<TypedExpr>, Arc<TypedExpr>),
    Match(Type, Arc<TypedExpr>, Option<Arc<Type>>, Vec<TypedMatchArm>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedMatchArm(pub TypedPat, pub Arc<TypedExpr>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedPat {
    CtorAt(Type, Ident, Vec<TypedPat>),
    Bind(Type, Ident),
    Else(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Referent {
    Binding(Ident),
    Component(Id<Component>),
}

pub trait Typed {
    fn typ(&self) -> Type;
}

impl Typed for TypedExpr {
    fn typ(&self) -> Type {
        match self {
            TypedExpr::Reference(typ, _) => typ.clone(),
            TypedExpr::Word(typ, _) => typ.clone(),
            TypedExpr::Bit(typ, _) => typ.clone(),
            TypedExpr::Struct(typ, _, _) => typ.clone(),
            TypedExpr::MethodCall(typ, _, _, _) => typ.clone(),
            TypedExpr::Field(typ, _, _) => typ.clone(),
            TypedExpr::Ctor(typ, _, _) => typ.clone(),
            TypedExpr::Idx(typ, _, _) => typ.clone(),
            TypedExpr::IdxRange(typ, _, _, _) => typ.clone(),
            TypedExpr::Cat(typ, _) => typ.clone(),
            TypedExpr::If(typ, _, _, _) => typ.clone(),
            TypedExpr::Match(typ, _subject, _ascription, _arms) => typ.clone(),
        }
    }
}

impl Typed for TypedPat {
    fn typ(&self) -> Type {
        match self {
            TypedPat::CtorAt(typ, _, _) => typ.clone(),
            TypedPat::Bind(typ, _) => typ.clone(),
            TypedPat::Else(typ) => typ.clone(),
        }
    }
}

impl Typed for TypedMatchArm {
    fn typ(&self) -> Type {
        let binding_typ = self.0.typ();
        let expr_typ = self.1.typ();
        assert_eq!(binding_typ, expr_typ);
        expr_typ
    }
}

impl Expr {
    pub fn subexprs(&self) -> Vec<Arc<Expr>> {
        let mut results = vec![];

        match self {
            Expr::MethodCall(s, _, es) => {
                results.push(s.clone());
                results.extend(es.iter().cloned().collect::<Vec<_>>());
            },
            Expr::Field(s, _) => {
                results.push(s.clone());
            },
            Expr::Ctor(_, es) => {
                results.extend(es.iter().cloned().collect::<Vec<_>>());
            },
            Expr::Idx(s, _) => {
                results.push(s.clone());
            },
            Expr::IdxRange(s, _, _) => {
                results.push(s.clone());
            },
            Expr::Cat(es) => {
                results.extend(es.iter().cloned().collect::<Vec<_>>());
            },
            Expr::If(s, a, b) => {
                results.push(s.clone());
                results.push(a.clone());
                results.push(b.clone());
            },
            Expr::Match(s, _, arms) => {
                results.push(s.clone());
                for MatchArm(_pat, e) in arms {
                    results.push(e.clone());
                }
            },
            Expr::Struct(_, assigns) => {
                for (_field, e) in assigns {
                    results.push(e.clone());
                }
            },
            Expr::Reference(_) => (),
            Expr::Word(_) => (),
            Expr::Bit(_) => (),
        }

        results
    }

    pub fn summary(&self) -> String {
        match self {
            Expr::Reference(_) => format!("Reference"),
            Expr::Word(_) => format!("Word"),
            Expr::Bit(_) => format!("Bit"),
            Expr::MethodCall(_, _, _) => format!("MethodCall"),
            Expr::Field(_, _) => format!("Field"),
            Expr::Struct(_, _) => format!("Struct"),
            Expr::Ctor(_, _) => format!("Ctor"),
            Expr::Idx(_, _) => format!("Idx"),
            Expr::IdxRange(_, _, _) => format!("IdxRange"),
            Expr::Cat(_) => format!("Cat"),
            Expr::If(_, _, _) => format!("If"),
            Expr::Match(_, _, _) => format!("Match"),
        }
    }
}
