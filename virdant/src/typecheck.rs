use std::sync::Arc;

use internment::Intern;

use crate::ast::Ast;
use crate::error::VirErr;
use crate::types::{Type, TypeScheme};
use crate::{CtorInfo, FieldInfo, ItemInfo, Virdant};
use crate::expr::{MatchArm, Pat, QualIdent, StaticIndex, Typed, TypedExpr, TypedMatchArm, TypedPat, WordLit};
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
        } else if let Ok(component) = self.virdant.resolve_component(x.as_ref(), self.moddef.as_item()) {
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
            Expr::Struct(struct_name, assigns) => self.infer_struct(*struct_name, assigns),
            Expr::MethodCall(subject, method, args) => self.infer_methodcall(subject.clone(), method.clone(), args),
            Expr::Field(subject, field) => self.infer_field(subject.clone(), field.clone()),
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
            Err(VirErr::TypeError(format!("Can't infer word with no width")))
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

    fn infer_struct(&self, struct_name: QualIdent, assigns: &[(Ident, Arc<Expr>)]) -> Result<Arc<TypedExpr>, VirErr> {
        let structdef = self.virdant.resolve_structdef(&struct_name, self.moddef.as_item())?;
        let structdef_info = &self.virdant.items[structdef.as_item()];
        let mut typed_assigns = vec![];
        for (field, expr) in assigns {
            let structdef_field = self.structdef_field(structdef_info, *field)?;
            let typed_expr = self.check(expr.clone(), *structdef_field.typ.unwrap())?;
            typed_assigns.push((field.clone(), typed_expr));
        }
        let typ = Type::structdef(structdef);
        Ok(Arc::new(TypedExpr::Struct(typ, struct_name, typed_assigns)))
    }

    fn structdef_field(&self, structdef_info: &ItemInfo, field_name: Ident) -> Result<&FieldInfo, VirErr> {
        for field in structdef_info.fields.unwrap() {
            let field_info = &self.virdant.fields[*field];
            if field_info.name == *field_name {
                return Ok(field_info);
            }
        }
        Err(VirErr::Other(format!("Unknown field {field_name}")))
    }

    fn infer_field(&self, subject: Arc<Expr>, field: Ident) -> Result<Arc<TypedExpr>, VirErr> {
        let typed_subject = self.infer(subject)?;
        let subject_typ = typed_subject.typ();
        let structdef  = if let TypeScheme::StructDef(structdef) = subject_typ.scheme() {
            structdef
        } else {
            return Err(VirErr::Other(format!("Not a struct type: {subject_typ}")));
        };

        let structdef_info = &self.virdant.items[structdef.as_item()];
        for field_id in structdef_info.fields.unwrap() {
            let field_info = &self.virdant.fields[*field_id];
            if field_info.name == *field {
                let typ = *field_info.typ.unwrap();
                return Ok(Arc::new(TypedExpr::Field(typ, typed_subject, field)));
            }
        }
        Err(VirErr::Other(format!("No such field {field} on {structdef}")))
    }

    fn check_ctor(&self, ctor: Ident, args: &[Arc<Expr>], expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        let uniondef  = if let TypeScheme::UnionDef(uniondef) = expected_typ.scheme() {
            uniondef
        } else {
            return Err(VirErr::Other(format!("Not a union type: {expected_typ}")));
        };
        let uniondef_info = &self.virdant.items[uniondef.as_item()];

        let mut typed_args = vec![];
        let ctor_info = self.uniondef_ctor(uniondef_info, ctor)?;
        let ctor_sig = ctor_info.sig.unwrap();

        if args.len() != ctor_sig.params().len() {
            return Err(VirErr::Other(format!("Incorrect number of args: found {} expected {}", args.len(), ctor_sig.params().len())));
        }

        for ((_param_name, param_typ), arg) in ctor_sig.params().iter().zip(args) {
            typed_args.push(self.check(arg.clone(), param_typ.clone())?);
        }

        Ok(Arc::new(TypedExpr::Ctor(expected_typ, ctor, typed_args)))
    }

    fn uniondef_ctor(&self, uniondef_info: &ItemInfo, ctor_name: Ident) -> Result<&CtorInfo, VirErr> {
        for ctor in uniondef_info.ctors.unwrap() {
            let ctor_info = &self.virdant.ctors[*ctor];
            if ctor_info.name == *ctor_name {
                return Ok(ctor_info);
            }
        }
        Err(VirErr::Other(format!("Unknown ctor {ctor_name}")))
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

    fn check_match(&self, subject: Arc<Expr>, ascription: Option<Ast>, arms: &[MatchArm], expected_typ: Type) -> Result<Arc<TypedExpr>, VirErr> {
        let typed_subject = if let Some(typ_ast) = ascription {
            let ascription_typ = self.virdant.resolve_type(typ_ast, self.moddef.as_item())?;
            self.check(subject, ascription_typ)?
        } else {
            self.infer(subject)?
        };

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
                let uniondef_info = &self.virdant.items[uniondef.as_item()];
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
