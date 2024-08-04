use std::sync::Arc;

use internment::Intern;

use crate::error::VirErr;
use crate::types::Type;
use crate::Virdant;
use crate::expr::{MatchArm, Pat, StaticIndex, Typed, TypedExpr, TypedMatchArm, TypedPat, WordLit};
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
            Expr::Word(wordlit) => self.check_word(wordlit, expected_typ),
            Expr::Struct(_, _) => self.check_struct(),
            Expr::Ctor(ctor, args) => self.check_ctor(*ctor, args, expected_typ),
            Expr::If(c, a, b) => self.check_if(c.clone(), a.clone(), b.clone(), expected_typ),
            Expr::Match(subject, ascription, match_arms) => self.check_match(subject.clone(), ascription.clone(), &match_arms, expected_typ),
            _ => {
                let typed_expr = self.infer(expr)?;
                let actual_typ = typed_expr.typ();
                if expected_typ != actual_typ {
                    Err(VirErr::TypeError(format!("Expected {expected_typ} but found {actual_typ}")))
                } else {
                    Ok(typed_expr)
                }
            },
        }
    }

    pub fn infer(&self, expr: Arc<Expr>) -> Result<Arc<TypedExpr>, VirErr> {
        match expr.as_ref() {
            Expr::Reference(path) => Ok(self.infer_reference(path).unwrap()),
            Expr::Word(wordlit) => self.infer_word(wordlit),
            Expr::Bit(bitlit) => self.infer_bit(*bitlit),
            Expr::MethodCall(subject, method, args) => self.infer_methodcall(subject.clone(), method.clone(), args),
            Expr::Cat(es) => self.infer_cat(es),
            Expr::Idx(subject, i) => self.infer_idx(subject.clone(), *i),
            Expr::IdxRange(subject, j, i) => self.infer_idxrange(subject.clone(), *j, *i),
            _ => Err(VirErr::CantInfer),
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

    fn infer_word(&self, wordlit: &WordLit) -> Result<Arc<TypedExpr>, VirErr> {
        if let Some(width) = wordlit.width {
            let typ = self.virdant.word_type(width);
            Ok(Arc::new(TypedExpr::Word(typ, wordlit.clone())))
        } else {
            Err(VirErr::TypeError(format!("")))
        }
    }

    fn infer_bit(&self, bit: bool) -> Result<Arc<TypedExpr>, VirErr> {
        let typ = self.virdant.bit_type();
        Ok(Arc::new(TypedExpr::Bit(typ, bit)))
    }

    fn infer_methodcall(&self, subject: Arc<Expr>, method: Ident, args: &[Arc<Expr>]) -> Result<Arc<TypedExpr>, VirErr> {
        let typed_subject = self.infer(subject)?;
        let subject_typ = typed_subject.typ();

        let method_sig = self.method_sig(subject_typ, method.clone())?;

        if args.len() != method_sig.params().len() {
            return Err(VirErr::Other(format!("Wrong number of arguments to method {method}")));
        }

        let mut typed_args = vec![];
        for (arg, param_typ) in args.iter().zip(method_sig.params()) {
            typed_args.push(self.check(arg.clone(), *param_typ)?);
        }
        let typ = method_sig.ret();
        Ok(Arc::new(TypedExpr::MethodCall(typ, typed_subject, method, typed_args)))
    }

    fn check_struct(&self) -> Result<Arc<TypedExpr>, VirErr> { todo!() }
    fn check_ctor(&self, ctor: Ident, args: &[Arc<Expr>], expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        let mut typed_args = vec![];
        for arg in args {
            // TODO should check after looking up ctor sig
            typed_args.push(self.infer(arg.clone())?);
        }

        Ok(Arc::new(TypedExpr::Ctor(expected_typ, ctor, typed_args)))
    }

    fn infer_idx(&self, subject: Arc<Expr>, i: StaticIndex) -> Result<Arc<TypedExpr>, VirErr> {
        let typed_subject = self.infer(subject)?;
        Ok(Arc::new(TypedExpr::Idx(self.virdant.bit_type(), typed_subject, i)))

    }

    fn infer_idxrange(&self, subject: Arc<Expr>, j: StaticIndex, i: StaticIndex) -> Result<Arc<TypedExpr>, VirErr> {
        if j < i {
            return Err(VirErr::Other(format!("First index must be greater or equal to the second")))
        }

        let width = j - i;

        let typed_subject = self.infer(subject)?;
        Ok(Arc::new(TypedExpr::IdxRange(self.virdant.word_type(width), typed_subject, j, i)))
    }

    fn infer_cat(&self, es: &[Arc<Expr>]) -> Result<Arc<TypedExpr>, VirErr> {
        let mut total_width = 0;
        let mut typed_es = vec![];
        for e in es {
            let typed_e = self.infer(e.clone())?;
            let e_typ = typed_e.typ();

            let width = if e_typ.is_word() {
                typed_e.typ().width()
            } else if e_typ.is_bit() {
                1
            } else {
                return Err(VirErr::Other(format!("Arguments to cat must be word or bits. Found {e_typ}.")));
            };

            total_width += width;
            typed_es.push(typed_e);
        }

        let typ = self.virdant.word_type(total_width);
        Ok(Arc::new(TypedExpr::Cat(typ, typed_es)))
    }

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

        let mut typed_arms = vec![];
        for arm in arms {
            let typed_pat = self.type_pat(arm.pat().clone(), subject_typ)?;
            let typed_expr = self.extend_with_pat(&typed_pat).check(arm.expr(), expected_typ.clone())?;
            let typed_arm = TypedMatchArm(typed_pat, typed_expr);
            typed_arms.push(typed_arm);
        }

        Ok(TypedExpr::Match(subject_typ.clone(), typed_subject, None, typed_arms).into())
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

    fn method_sig(&self, typ: Type, method: Ident) -> Result<MethodSig, VirErr> {
        if typ.is_word() {
            let n = typ.width();
            if *method == "add" {
                Ok(MethodSig(vec![typ.clone()], typ.clone()))
            } else if *method == "inc" {
                Ok(MethodSig(vec![], typ.clone()))
            } else if *method == "dec" {
                Ok(MethodSig(vec![], typ.clone()))
            } else if *method == "sll" {
                Ok(MethodSig(vec![typ.clone()], typ.clone()))
            } else if *method == "srl" {
                Ok(MethodSig(vec![typ.clone()], typ.clone()))
            } else if *method == "sub" {
                Ok(MethodSig(vec![typ.clone()], typ.clone()))
            } else if *method == "and" {
                Ok(MethodSig(vec![typ.clone()], typ.clone()))
            } else if *method == "or" {
                Ok(MethodSig(vec![typ.clone()], typ.clone()))
            } else if *method == "xor" {
                Ok(MethodSig(vec![typ.clone()], typ.clone()))
            } else if *method == "lt" {
                Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
            } else if *method == "lte" {
                Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
            } else if *method == "gt" {
                Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
            } else if *method == "gte" {
                Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
            } else if *method == "eq" {
                Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
            } else if *method == "neq" {
                Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
            } else if *method == "not" {
                Ok(MethodSig(vec![], typ.clone()))
            } else if is_pow2(n) && *method == "get" {
                let argtyp = self.virdant.word_type(clog2(n));
                Ok(MethodSig(vec![argtyp.clone()], self.virdant.bit_type()))
            } else {
                Err(VirErr::Other(format!("No such method {method} for type {typ}")))
            }
        } else {
            Err(VirErr::Other(format!("No such method {method} for type {typ}")))
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MethodSig(Vec<Type>, Type);

impl MethodSig {
    pub fn params(&self) -> &[Type] {
        &self.0
    }

    pub fn ret(&self) -> Type {
        self.1
    }
}

fn pow(n: u64, k: u64) -> u64 {
    let mut p = 1;
    for _ in 0..k {
        p *= n
    }
    p
}

fn clog2(n: u64) -> u64 {
    let mut result = 0;
    while n > (1 << result) {
        result += 1;
    }
    result
}

fn is_pow2(n: u64) -> bool {
    n != 0 && (n & (n - 1)) == 0
}
