use std::sync::Arc;

use internment::Intern;

use crate::ast::Ast;
use crate::{common::*, ItemKind};
use crate::error::VirErr;
use crate::types::{Type, TypeScheme};
use crate::{FieldInfo, ItemInfo, Virdant};
use crate::ast::expr::{MatchArm, Pat, QualIdent, TypedPat, WordLit};
use crate::ast::expr::Expr;
use crate::ast::expr::Ident;
use crate::id::*;
use crate::context::Context;

#[derive(Debug)]
pub struct TypingContext<'a> {
    virdant: &'a mut Virdant,
    item: Id<Item>,
    context: Context<Ident, Type>,
}

impl<'a> TypingContext<'a> {
    pub fn new(virdant: &'a mut Virdant, item: Id<Item>) -> TypingContext<'a> {
        let item_info = &virdant.items[item];
        let kind = *item_info.kind.unwrap();
        match kind {
            ItemKind::ModDef => Self::new_from_moddef(virdant, item.cast()),
            ItemKind::FnDef => Self::new_from_fndef(virdant, item.cast()),
            _ => panic!("Can't typecheck an expression in item of kind {kind:?}"),
        }
    }

    fn new_from_moddef(virdant: &'a mut Virdant, moddef: Id<ModDef>) -> TypingContext<'a> {
        TypingContext {
            virdant,
            item: moddef.as_item(),
            context: Context::empty(),
        }
    }

    fn new_from_fndef(virdant: &'a mut Virdant, fndef: Id<FnDef>) -> TypingContext<'a> {
        let item = fndef.as_item();

        let fndef_info = &virdant.items[item];
        let fnsig = fndef_info.sig.unwrap();

        let mut context = Context::empty();
        for (param_name, param_typ) in fnsig.params() {
            context = context.extend(Ident::new(param_name.clone()), param_typ.clone());
        }

        TypingContext {
            virdant,
            item,
            context,
        }
    }

    fn lookup(&self, x: Intern<String>) -> Lookup {
        if let Some(val) = self.context.lookup(&x) {
            Lookup::Binding(val)
        } else if let Ok(component) = self.virdant.resolve_component(x.as_ref(), self.item) {
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
    pub fn check(&mut self, exprroot: Id<ExprRoot>, expected_typ: Type) -> Result<(), VirErr> {
        let expr = &self.virdant.exprroots[exprroot].ast.unwrap();
        let result = match expr.as_ref() {
            Expr::Word(_span, wordlit) => self.check_word(exprroot, wordlit, expected_typ),
            Expr::Ctor(_span, ctor, _) => self.check_ctor(exprroot, *ctor, expected_typ),
            Expr::Enumerant(_span, enumerant) => self.check_enumerant(exprroot, *enumerant, expected_typ),
            Expr::If(_span, _, _, _) => self.check_if(exprroot, expected_typ),
            Expr::Match(_span, _subject, ascription, arms) => self.check_match(exprroot, ascription.clone(), arms.clone(), expected_typ),
            Expr::Zext(_span, _arg) => self.check_zext(exprroot, expected_typ),
            Expr::Sext(_span, _arg) => self.check_sext(exprroot, expected_typ),
            _ => {
                match self.infer(exprroot) {
                    Err(e) => Err(e),
                    Ok(actual_typ) => {
                        if expected_typ != actual_typ {
                            Err(VirErr::TypeError(format!("Expected {expected_typ} but found {actual_typ}")))
                        } else {
                            Ok(())
                        }
                    },
                }
            },
        };

        match result {
            Ok(()) => {
                let exprroot_info = &mut self.virdant.exprroots[exprroot];
                exprroot_info.typ.set(expected_typ);
                Ok(())
            },
            Err(e) => Err(e),
        }
    }

    pub fn infer(&mut self, exprroot: Id<ExprRoot>) -> Result<Type, VirErr> {
        let expr = &self.virdant.exprroots[exprroot].ast.unwrap();
        let result = match expr.as_ref() {
            Expr::Reference(_span, path) => {
                let path = Intern::new(path.to_vec().into_iter().map(|s| s.to_string()).collect::<Vec<_>>().join("."));
                match self.lookup(path) {
                    Lookup::NotFound => Err(VirErr::Other(format!("Not Found: {path}"))),
                    Lookup::Binding(typ) => Ok(typ),
                    Lookup::Component(component, typ) => {
                        let exprroot_info = &mut self.virdant.exprroots[exprroot];
                        exprroot_info.reference_component = Some(component);
                        Ok(typ)
                    },
                }
            },
            Expr::Word(_span, wordlit) => self.infer_word(wordlit),
            Expr::Bit(_span, _bitlit) => self.infer_bit(),
            Expr::Idx(_span, _subject, i) => self.infer_idx(exprroot, *i),
            Expr::IdxRange(_span, _subject, j, i) => self.infer_idxrange(exprroot, *j, *i),
            Expr::MethodCall(_span, _subject, method, _args) => self.infer_methodcall(exprroot, *method),
            Expr::FnCall(_span, fndef, _args) => self.infer_fncall(exprroot, *fndef),
            Expr::Field(_span, _subject, field) => self.infer_field(exprroot, field.clone()),
            Expr::Cat(_span, _es) => self.infer_cat(exprroot),
            Expr::Struct(_span, struct_name, assigns) => self.infer_struct(exprroot, *struct_name, assigns.to_vec()),
            _ => Err(VirErr::CantInfer),
        };

        match result {
            Ok(typ) => {
                let exprroot_info = &mut self.virdant.exprroots[exprroot];
                exprroot_info.typ.set(typ);
                Ok(typ)
            },
            Err(e) => Err(e),
        }
    }

    fn infer_cat(&mut self, exprroot: Id<ExprRoot>) -> Result<Type, VirErr> {
        let es = self.virdant.exprroots[exprroot].children.clone();

        let mut total_width = 0;
        for e in es {
            let e_typ  = self.infer(e)?;

            let width = if e_typ.is_word() {
                e_typ.width()
            } else if e_typ.is_bit() {
                1
            } else {
                return Err(VirErr::Other(format!("Arguments to cat must be word or bits. Found {e_typ}.")));
            };

            total_width += width;
        }

        let typ = self.virdant.word_type(total_width);
        Ok(typ)
    }

    fn infer_idxrange(&mut self, exprroot: Id<ExprRoot>, j: StaticIndex, i: StaticIndex) -> Result<Type, VirErr> {
        let subject = &self.virdant.exprroots[exprroot].children[0];
        let subject_typ = self.infer(*subject)?;

        if j < i {
            return Err(VirErr::Other(format!("First index must be greater or equal to the second")))
        } else if !subject_typ.is_word() {
            Err(VirErr::TypeError("Can't index into non-Word".to_string()))
        } else if j > subject_typ.width() {
            Err(VirErr::TypeError(format!("Index too big: {j} > {width}", width = subject_typ.width())))
        } else {
            let width = j - i;
            Ok(self.virdant.word_type(width))
        }
    }

    fn infer_idx(&mut self, exprroot: Id<ExprRoot>, i: StaticIndex) -> Result<Type, VirErr> {
        let subject = &self.virdant.exprroots[exprroot].children[0];
        let subject_typ = self.infer(*subject)?;
        if !subject_typ.is_word() {
            Err(VirErr::TypeError("Can't index into non-Word".to_string()))
        } else if i >= subject_typ.width() {
            Err(VirErr::TypeError(format!("Index too big: {i} >= {width}", width = subject_typ.width())))
        } else {
            Ok(self.virdant.bit_type())
        }
    }

    fn check_if(&mut self, exprroot: Id<ExprRoot>, expected_typ: Type) -> Result<(), VirErr> {
        let bit_typ = self.virdant.bit_type();

        let subject = self.virdant.exprroots[exprroot].children[0];
        let a = self.virdant.exprroots[exprroot].children[1];
        let b = self.virdant.exprroots[exprroot].children[2];

        self.check(subject, bit_typ)?;
        self.check(a, expected_typ.clone())?;
        self.check(b, expected_typ.clone())?;
        Ok(())
    }

    fn check_word(&self, exprroot: Id<ExprRoot>, wordlit: &WordLit, expected_typ: Type) -> Result<(), VirErr> {
        if let Some(width) = wordlit.width {
            let actual_typ = self.virdant.word_type(width);
            if expected_typ == actual_typ {
                Ok(())
            } else {
                Err(VirErr::TypeError(format!("Does not match: {expected_typ} and {actual_typ}")))
            }
        } else {
            if expected_typ.is_word() {
                let n = expected_typ.width();
                if wordlit.value < pow(2, n) {
                    Ok(())
                } else {
                    Err(VirErr::TypeError(format!("Doesn't fit")))
                }
            } else {
                let exprroot_info = &self.virdant.exprroots[exprroot];
                let span = exprroot_info.span.unwrap();
                Err(VirErr::TypeError(format!("Expected Word type: {:?} to {:?}", span.start(), span.end())))
            }
        }
    }

    fn check_zext(&mut self, exprroot: Id<ExprRoot>, expected_typ: Type) -> Result<(), VirErr> {
        if expected_typ.is_word() {
            let expr = self.virdant.exprroots[exprroot].children[0].clone();
            let expr_typ  = self.infer(expr)?;

            if !expected_typ.is_word() {
                return Err(VirErr::TypeError(format!("Argument of zext must be a word.")))
            }

            if expr_typ.width() > expected_typ.width() {
                return Err(VirErr::TypeError(format!("Argument of zext must be a smaller word size than what it is being extended to.")))
            }

            Ok(())
        } else {
            Err(VirErr::TypeError(format!("Can't zext to {expected_typ}")))
        }
    }

    fn check_sext(&mut self, exprroot: Id<ExprRoot>, expected_typ: Type) -> Result<(), VirErr> {
        if expected_typ.is_word() {
            let expr = self.virdant.exprroots[exprroot].children[0].clone();
            let expr_typ  = self.infer(expr)?;

            if !expected_typ.is_word() {
                return Err(VirErr::TypeError(format!("Argument of sext must be a word.")))
            }

            if expr_typ.width() > expected_typ.width() {
                return Err(VirErr::TypeError(format!("Argument of sext must be a smaller word size than what it is being extended to.")))
            }

            Ok(())
        } else {
            Err(VirErr::TypeError(format!("Can't sext to {expected_typ}")))
        }
    }

    fn infer_word(&self, wordlit: &WordLit) -> Result<Type, VirErr> {
        if let Some(width) = wordlit.width {
            Ok(self.virdant.word_type(width))
        } else {
            Err(VirErr::TypeError(format!("Can't infer word with no width")))
        }
    }

    fn infer_bit(&self) -> Result<Type, VirErr> {
        Ok(self.virdant.bit_type())
    }

    fn check_ctor(&mut self, exprroot: Id<ExprRoot>, ctor: Ident, expected_typ: Type) -> Result<(), VirErr> {
        let exprroot_info = &mut self.virdant.exprroots[exprroot];
        let args = exprroot_info.children.clone();

        let uniondef  = if let TypeScheme::UnionDef(uniondef) = expected_typ.scheme() {
            uniondef
        } else {
            return Err(VirErr::Other(format!("Not a union type: {expected_typ}")));
        };
        let uniondef_info = self.virdant.items[uniondef.as_item()].clone();

        let ctor = self.uniondef_ctor(&uniondef_info, ctor)?;
        let ctor_info = self.virdant.ctors[ctor].clone();
        let ctor_sig = ctor_info.sig.unwrap();

        if args.len() != ctor_sig.params().len() {
            return Err(VirErr::Other(format!("Incorrect number of args: found {} expected {}", args.len(), ctor_sig.params().len())));
        }

        for ((_param_name, param_typ), arg) in ctor_sig.params().iter().zip(args) {
            self.check(arg.clone(), param_typ.clone())?;
        }

        Ok(())
    }

    fn check_enumerant(&mut self, _exprroot: Id<ExprRoot>, enumerant: Ident, expected_typ: Type) -> Result<(), VirErr> {
        let enumdef  = if let TypeScheme::EnumDef(enumdef) = expected_typ.scheme() {
            enumdef
        } else {
            return Err(VirErr::Other(format!("Not an enum type: {expected_typ}")));
        };
        let enumdef_info = self.virdant.items[enumdef.as_item()].clone();

        let enumerant = self.enumdef_enumerant(&enumdef_info, enumerant)?;
        let _enumerant_info = self.virdant.enumerants[enumerant].clone();

        Ok(())
    }

    fn uniondef_ctor(&self, uniondef_info: &ItemInfo, ctor: Ident) -> Result<Id<Ctor>, VirErr> {
        for ctor_id in uniondef_info.ctors.unwrap().iter() {
            let ctor_info = &self.virdant.ctors[*ctor_id];
            if ctor_info.name == *ctor {
                return Ok(*ctor_id);
            }

        }
        Err(VirErr::Other("No such ctor".to_string()))
    }

    fn enumdef_enumerant(&self, enumdef_info: &ItemInfo, enumerant: Ident) -> Result<Id<Enumerant>, VirErr> {
        for enumerant_id in enumdef_info.enumerants.unwrap().iter() {
            let enumerant_info = &self.virdant.enumerants[*enumerant_id];
            if enumerant_info.name == *enumerant {
                return Ok(*enumerant_id);
            }

        }
        Err(VirErr::Other(format!("No such enumerant: {enumerant}")))
    }

    fn infer_methodcall(&mut self, exprroot: Id<ExprRoot>, method: Ident) -> Result<Type, VirErr> {
        let subexprs = self.virdant.exprroots[exprroot].children.clone();
        let subject = subexprs[0].clone();
        let args = subexprs[1..].to_vec();

        let subject_typ = self.infer(subject)?;

        let method_sig = self.method_sig(subject_typ, method.clone())?;

        if args.len() != method_sig.params().len() {
            return Err(VirErr::Other(format!("Wrong number of arguments to method {method}")));
        }

        for (arg, param_typ) in args.iter().zip(method_sig.params()) {
            self.check(arg.clone(), *param_typ)?;
        }
        let typ = method_sig.ret();
        Ok(typ)
    }

    fn infer_fncall(&mut self, exprroot: Id<ExprRoot>, fndef: Ident) -> Result<Type, VirErr> {
        let args = self.virdant.exprroots[exprroot].children.clone();

        let fndef = self.virdant.resolve_fndef(&fndef, self.item)?;
        let fndef_info = self.virdant.items[fndef.as_item()].clone();

        let fn_sig = fndef_info.sig.unwrap();
        let typ = fn_sig.ret();

        if args.len() != fn_sig.params().len() {
            return Err(VirErr::Other(format!("Wrong number of arguments to function {fndef}")));
        }

        for (arg, (_param_name, param_typ)) in args.iter().zip(fn_sig.params()) {
            self.check(arg.clone(), *param_typ)?;
        }

        let exprroot_info = &mut self.virdant.exprroots[exprroot];
        exprroot_info.fncall_fndef = Some(fndef);

        Ok(typ)
    }

    fn infer_struct(&mut self, exprroot: Id<ExprRoot>, struct_name: QualIdent, assigns: Vec<(Ident, Arc<Expr>)>) -> Result<Type, VirErr> {
        let args = self.virdant.exprroots[exprroot].children.clone();

        let structdef = self.virdant.resolve_structdef(&struct_name, self.item)?;
        let structdef_info = self.virdant.items[structdef.as_item()].clone();
        for ((field, _expr), arg) in assigns.iter().zip(args) {
            let structdef_field = self.structdef_field(&structdef_info, *field)?;
            self.check(arg, *structdef_field.typ.unwrap())?;
        }

        let exprroot_info = &mut self.virdant.exprroots[exprroot];
        exprroot_info.struct_structdef = Some(structdef);

        let typ = Type::structdef(structdef);
        Ok(typ)
    }

    fn structdef_field(&self, structdef_info: &ItemInfo, field_name: Ident) -> Result<FieldInfo, VirErr> {
        for field in structdef_info.fields.unwrap() {
            let field_info = &self.virdant.fields[*field];
            if field_info.name == *field_name {
                return Ok(field_info.clone());
            }
        }
        Err(VirErr::Other(format!("Unknown field {field_name}")))
    }

    fn infer_field(&mut self, exprroot: Id<ExprRoot>, field: Ident) -> Result<Type, VirErr> {
        let subexprs = &self.virdant.exprroots[exprroot].children;
        let subject = subexprs[0].clone();

        let subject_typ = self.infer(subject)?;
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
                return Ok(typ);
            }
        }
        Err(VirErr::Other(format!("No such field {field} on {structdef}")))
    }


    fn check_match(&mut self, exprroot: Id<ExprRoot>, ascription: Option<Ast>, arms: Vec<MatchArm>, expected_typ: Type) -> Result<(), VirErr> {
        let subexprs = &self.virdant.exprroots[exprroot].children;
        let subject = subexprs[0].clone();
        let arm_exprroots = subexprs[1..].to_vec();

        let subject_typ = if let Some(typ_ast) = ascription {
            let ascription_typ = self.virdant.resolve_type(typ_ast, self.item)?;
            self.check(subject, ascription_typ)?;
            ascription_typ
        } else {
            self.infer(subject)?
        };

        for (arm, arm_exprroot) in arms.iter().zip(arm_exprroots) {
            let typed_pat = self.type_pat(arm.pat().clone(), subject_typ)?;
            self.extend_with_pat(&typed_pat).check(arm_exprroot, expected_typ.clone())?;
        }

        Ok(())
    }

    fn type_pat(&self, pat: Pat, typ: Type) -> Result<TypedPat, VirErr> {
        match pat {
            Pat::CtorAt(ctor_name, pats) => {
                let uniondef = match typ.scheme() {
                    crate::types::TypeScheme::StructDef(_structdef) => return Err(VirErr::InvalidPat(format!(""))),
                    crate::types::TypeScheme::BuiltinDef(_builtindef) => return Err(VirErr::InvalidPat(format!(""))),
                    crate::types::TypeScheme::EnumDef(_enumdef) => return Err(VirErr::InvalidPat(format!(""))),
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
            Pat::EnumerantAt(enumerant_name) => {
                let enumdef = match typ.scheme() {
                    crate::types::TypeScheme::StructDef(_structdef) => return Err(VirErr::InvalidPat(format!(""))),
                    crate::types::TypeScheme::BuiltinDef(_builtindef) => return Err(VirErr::InvalidPat(format!(""))),
                    crate::types::TypeScheme::EnumDef(enumdef) => enumdef,
                    crate::types::TypeScheme::UnionDef(_uniondef) => return Err(VirErr::InvalidPat(format!(""))),
                };
                let enumdef_info = &self.virdant.items[enumdef.as_item()];
                let enumerants = &enumdef_info.enumerants.unwrap();

                let enumerant_opt = enumerants.into_iter().find(|enumerant| {
                    let ctor_info = &self.virdant.enumerants[**enumerant];
                    ctor_info.name == *enumerant_name
                });

                if enumerant_opt.is_none() {
                    return Err(VirErr::Other(format!("No such enumerant {enumerant_name} on {typ}")));
                }

                Ok(TypedPat::EnumerantAt(typ.clone(), enumerant_name))
            },
            Pat::Bind(x) => Ok(TypedPat::Bind(typ.clone(), x.clone())),
            Pat::Else => Ok(TypedPat::Else(typ.clone())),
        }
    }

    fn duplicate<'b>(&'b mut self) -> TypingContext<'b>
        where 'a: 'b {
        TypingContext {
            virdant: self.virdant,
            item: self.item,
            context: self.context.clone(),
        }
    }

    fn extend_with_pat<'b>(&'b mut self, pat: &TypedPat) -> TypingContext<'b>
        where 'a: 'b {
        let extension = TypingContext::pat_to_extension(pat);
        let mut new_context = self.context.clone();
        for (x, typ) in extension {
            new_context = new_context.extend(x, typ);
        }

        let mut result = self.duplicate();
        result.context = new_context;
        result
    }

    fn pat_to_extension(pat: &TypedPat) -> Vec<(Ident, Type)> {
        match pat {
            TypedPat::Bind(typ, x) => {
                vec![(*x, *typ)]
            },
            TypedPat::Else(_) => vec![],
            TypedPat::EnumerantAt(_typ, _ctor) => vec![],
            TypedPat::CtorAt(_typ, _ctor, pats) => {
                let mut results = vec![];
                for pat in pats {
                    results.extend(TypingContext::pat_to_extension(pat));
                }
                results
            },
        }
    }

    fn method_sig(&self, typ: Type, method: Ident) -> Result<MethodSig, VirErr> {
        match typ.scheme() {
            TypeScheme::StructDef(_) => {
                Err(VirErr::Other(format!("No such method {method} for type {typ}")))
            },
            TypeScheme::UnionDef(_) => {
                Err(VirErr::Other(format!("No such method {method} for type {typ}")))
            },
            TypeScheme::EnumDef(enumdef) => {
                if *method == "value" {
                    let enumdef_info = &self.virdant.items[enumdef.as_item()];
                    let width = *enumdef_info.width.unwrap();
                    Ok(MethodSig(vec![], self.virdant.word_type(width)))
                } else {
                    Err(VirErr::Other(format!("No such method {method} for type {typ}")))
                }
            },
            TypeScheme::BuiltinDef(_) => {
                if typ.is_bit() {
                    if *method == "eq" {
                        Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
                    } else if *method == "neq" {
                        Ok(MethodSig(vec![typ.clone()], self.virdant.bit_type()))
                    } else if *method == "not" {
                        Ok(MethodSig(vec![], typ.clone()))
                    } else if *method == "and" {
                        Ok(MethodSig(vec![typ.clone()], typ.clone()))
                    } else if *method == "or" {
                        Ok(MethodSig(vec![typ.clone()], typ.clone()))
                    } else if *method == "xor" {
                        Ok(MethodSig(vec![typ.clone()], typ.clone()))
                    } else {
                        Err(VirErr::Other(format!("No such method {method} for type {typ}")))
                    }
                } else if typ.is_word() {
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
            },
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
