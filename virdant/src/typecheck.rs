use std::sync::Arc;

use internment::Intern;

use crate::error::VirErr;
use crate::types::Type;
use crate::Virdant;
use crate::expr::{MatchArm, Pat, Typed, TypedExpr, TypedMatchArm, TypedPat, WordLit};
use crate::expr::Referent;
use crate::expr::Expr;
use crate::expr::Ident;
use crate::id::*;
use crate::context::Context;

#[derive(Clone, Debug)]
pub struct TypingContext<'a> {
    virdant: &'a Virdant,
    moddef: Id<ModDef>,
    context: Context<Ident, Type>,
}

impl<'a> TypingContext<'a> {
    pub fn new(virdant: &'a Virdant, moddef: Id<ModDef>) -> TypingContext<'a> {
        TypingContext {
            virdant,
            moddef,
            context: Context::empty(),
        }
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
            Expr::Bit(bitlit) => self.check_bit(*bitlit, expected_typ),
            Expr::MethodCall(_, _, _) => self.check_methodcall(),
            Expr::Struct(_, _) => self.check_struct(),
            Expr::Ctor(ctor, args) => self.check_ctor(*ctor, args, expected_typ),
            Expr::Idx(_, _) => self.check_idx(),
            Expr::IdxRange(_, _, _) => self.check_idxrange(),
            Expr::Cat(_) => self.check_cat(),
            Expr::If(c, a, b) => self.check_if(c.clone(), a.clone(), b.clone(), expected_typ),
            Expr::Match(subject, ascription, match_arms) => self.check_match(subject.clone(), ascription.clone(), &match_arms, expected_typ),
        }
    }

    fn check_reference(&self, expr: Arc<Expr>, expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        let path = if let Expr::Reference(path) = expr.as_ref() {
            path.into_iter().map(|s| s.as_ref().to_owned()).collect::<Vec<_>>().join(".")
        } else {
            unreachable!()
        };

        let typed_expr = self.infer(expr.clone())?;
        let actual_typ = typed_expr.typ();
        if expected_typ != actual_typ {
            Err(VirErr::TypeError(format!("Wrong types: {path} is {expected_typ} vs {actual_typ}")))
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

    fn check_bit(&self, bitlit: bool, expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        if expected_typ.is_bit() {
            Ok(Arc::new(TypedExpr::Bit(expected_typ, bitlit)))
        } else {
            Err(VirErr::TypeError(format!("Expected Bit type")))
        }
    }

    fn check_methodcall(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_struct(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_ctor(&self, ctor: Ident, args: &[Arc<Expr>], expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        let mut typed_args = vec![];
        for arg in args {
            // TODO should check after looking up ctor sig
            typed_args.push(self.infer(arg.clone())?);
        }

        Ok(Arc::new(TypedExpr::Ctor(expected_typ, ctor, typed_args)))
    }
    fn check_idx(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_idxrange(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_cat(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }

    fn check_if(&self, c: Arc<Expr>, a: Arc<Expr>, b: Arc<Expr>, expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        let bit_typ = self.virdant.bit_type();
        let typed_c = self.check(c, bit_typ)?;
        let typed_a = self.check(a, expected_typ.clone())?;
        let typed_b = self.check(b, expected_typ.clone())?;
        Ok(TypedExpr::If(expected_typ, typed_c, typed_a, typed_b).into())
    }

    fn check_match(&self, subject: Arc<Expr>, _ascription: Option<()>, arms: &[MatchArm], expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
/*
        let typed_subject = if let Some(ascription_typ) = ascription {
            let ascription_typ = db.resolve_typ(ascription_typ.clone(), moddef_id.package())?;
            db.typecheck_expr(moddef_id.clone(), subject.clone(), ascription_typ, ctx.clone())?
        } else {
             db.typeinfer_expr(moddef_id.clone(), subject.clone(), ctx.clone())?
        };
*/

        let typed_subject = self.infer(subject)?;
        let subject_typ = typed_subject.typ();

        let pats: Vec<Pat> = arms.iter().map(|arm| arm.pat().clone()).collect();
        dbg!(&pats);

        let mut typed_arms = vec![];
        for arm in arms {
            let typed_pat = self.type_pat(arm.pat().clone(), subject_typ)?;
            let typed_expr = self.extend_with_pat(&typed_pat).check(arm.expr(), expected_typ.clone())?;
            let typed_arm = TypedMatchArm(typed_pat, typed_expr);
            typed_arms.push(typed_arm);
        }

        Ok(TypedExpr::Match(subject_typ.clone(), typed_subject, None, typed_arms).into())
    }

    pub fn infer(&self, expr: Arc<Expr>) -> Result<Arc<TypedExpr>, VirErr> {
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
            Lookup::NotFound => Err(VirErr::Other(format!("Not Found: {path}"))),
        }
    }

    fn type_pat(&self, pat: Pat, typ: Type) -> Result<TypedPat, VirErr> {
        match pat {
            Pat::CtorAt(ctor_name, pats) => {
                let uniondef = match typ.scheme() {
                    crate::types::TypeScheme::StructDef(_structdef) => return Err(VirErr::InvalidPat(format!(""))),
                    crate::types::TypeScheme::BuiltinDef(_builtindef) => return Err(VirErr::InvalidPat(format!(""))),
                    crate::types::TypeScheme::UnionDef(uniondef) => uniondef,
                };
                let uniondef_info = &self.virdant.uniondefs[uniondef];
                let ctors = &uniondef_info.ctors.unwrap();

                let ctor_opt = ctors.into_iter().find(|ctor| {
                    let ctor_info = &self.virdant.ctors[**ctor];
                    ctor_info.name == *ctor_name
                });

                if ctor_opt.is_none() {
                    return Err(VirErr::Other(format!("No such ctor {ctor_name} on {typ}")));
                }

                let ctor = *ctor_opt.unwrap();
                let ctor_info = &self.virdant.ctors[ctor];
                let sig = ctor_info.sig.unwrap();
                if pats.len() != sig.params().len() {
                    return Err(VirErr::Other(format!("Expected {} params on {ctor}", sig.params().len())));
                }

                let mut typed_pats = vec![];
                for (pat, (_param_name, param_typ)) in pats.into_iter().zip(sig.params()) {
                    let typed_pat = self.type_pat(pat, *param_typ)?;
                    typed_pats.push(typed_pat);
                }

                Ok(TypedPat::CtorAt(typ.clone(), ctor_name, typed_pats))
            },
            Pat::Bind(x) => Ok(TypedPat::Bind(typ.clone(), x.clone())),
            Pat::Else => Ok(TypedPat::Else(typ.clone())),
        }
    }

    fn extend(&self, x: Ident, typ: Type) -> TypingContext<'a> {
        let mut result = self.clone();
        result.context = result.context.extend(x, typ);
        result
    }

    fn extend_with_pat(&self, pat: &TypedPat) -> TypingContext<'a> {
        match pat {
            TypedPat::Bind(typ, x) => {
                self.extend(*x, *typ)
            },
            TypedPat::Else(_) => self.clone(),
            TypedPat::CtorAt(_typ, _ctor, pats) => {
                let mut new_context = self.clone();
                for pat in pats {
                    new_context = new_context.extend_with_pat(pat);
                }
                new_context
            },
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
