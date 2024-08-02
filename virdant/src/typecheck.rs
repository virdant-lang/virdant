use std::sync::Arc;

use internment::Intern;

use crate::error::VirErr;
use crate::types::{Type, TypeScheme};
use crate::Virdant;
use crate::expr::{Typed, TypedExpr, WordLit};
use crate::expr::Referent;
use crate::expr::Expr;
use crate::id::*;
use crate::context::Context;

#[derive(Clone, Debug)]
pub struct TypingContext<'a> {
    virdant: &'a Virdant,
    moddef: Id<ModDef>,
    context: Context<Intern<String>, Type>,
}

impl<'a> TypingContext<'a> {
    pub fn new(virdant: &'a Virdant, moddef: Id<ModDef>) -> TypingContext<'a> {
        TypingContext {
            virdant,
            moddef,
            context: Context::empty(),
        }
    }

    pub fn push(&self, x: Intern<String>, typ: Type) -> TypingContext<'a> {
        let mut result = self.clone();
        result.context = result.context.extend(x, typ);
        result
    }

    fn lookup(&self, x: Intern<String>) -> Lookup {
        if let Some(val) = self.context.lookup(&x) {
            Lookup::Binding(val)
        } else if let Ok(component) = self.virdant.resolve_component(x.as_ref(), self.moddef) {
            Lookup::Component(component, self.virdant.components[component].typ.unwrap().clone())
        } else {
            Lookup::NotFound
        }
    }
}

#[derive(Clone, Debug)]
enum Lookup {
    Binding(Type),
    Component(Id<Component>, Type),
    NotFound,
}

impl<'a> TypingContext<'a> {
    pub fn check(&self, expr: Arc<Expr>, expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        match expr.as_ref() {
            Expr::Reference(_path) => self.check_reference(expr.clone(), expected_typ),
            Expr::Word(wordlit) => self.check_word(wordlit, expected_typ),
            Expr::MethodCall(_, _, _) => self.check_methodcall(),
            Expr::Struct(_, _) => self.check_struct(),
            Expr::Ctor(_, _) => self.check_ctor(),
            Expr::Idx(_, _) => self.check_idx(),
            Expr::IdxRange(_, _, _) => self.check_idxrange(),
            Expr::Cat(_) => self.check_cat(),
            Expr::If(_, _, _) => self.check_if(),
            Expr::Match(_, _, _) => self.check_match(),
        }
    }

    fn check_reference(&self, expr: Arc<Expr>, expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        let typed_expr = self.typeinfer(expr.clone())?;
        let actual_typ = typed_expr.typ();
        if expected_typ != actual_typ {
//            Err(VirErr::TypeError(format!("Wrong types: {path} is {typ} vs {actual_typ}")))
            todo!()
        } else {
            Ok(typed_expr)
        }
    }

    fn check_word(&self, wordlit: &WordLit, expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        if let Some(width) = wordlit.width {
            let actual_typ = self.virdant.word_type(width);
            if expected_typ == actual_typ {
                Ok(TypedExpr::Word(expected_typ, wordlit.clone()).into())
            } else {
                Err(VirErr::TypeError(format!("Does not match: {expected_typ} and {actual_typ}")))
            }
        } else {
            if expected_typ.is_word() {
                let n = expected_typ.width();
                if wordlit.value < pow(2, n) {
                    Ok(TypedExpr::Word(expected_typ, wordlit.clone()).into())
                } else {
                    Err(VirErr::TypeError(format!("Doesn't fit")))
                }
            } else {
                    Err(VirErr::TypeError(format!("Expected Word type")))
            }
        }
    }

    fn check_methodcall(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_struct(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_ctor(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_idx(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_idxrange(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_cat(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_if(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_match(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }

    pub fn typeinfer(&self, expr: Arc<Expr>) -> Result<Arc<TypedExpr>, VirErr> {
        match expr.as_ref() {
            Expr::Reference(path) => Ok(self.infer_reference(path).unwrap()),
            _ => todo!(),
        }
    }

    fn infer_reference(&self, path: &[Intern<String>]) -> Result<Arc<TypedExpr>, VirErr> {
        let path = Intern::new(path.to_vec().into_iter().map(|s| s.to_string()).collect::<Vec<_>>().join("."));
        match self.lookup(path) {
            Lookup::Binding(actual_typ) => Ok(TypedExpr::Reference(actual_typ, Referent::Binding(path)).into()),
            Lookup::Component(component, actual_typ) => Ok(TypedExpr::Reference(actual_typ, Referent::Component(component)).into()),
            Lookup::NotFound => todo!(),
        }
    }
}

fn pow(n: u64, k: u64) -> u64 {
    let mut p = 1;
    for _ in 0..k {
        p *= n
    }
    p
}
