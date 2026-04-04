use indexmap::IndexMap;

use crate::common::Width;
use crate::types::Type;
use crate::sim::expr::{Expr, Referent};
use crate::analysis::symbols::SymbolId;

#[derive(Clone, Debug)]
pub enum Value {
    X(Type),
    Z(Type),
    Bit(bool),
    Word(Width, u64), // Should be Value from common
    Ctor(Type, SymbolId, Vec<Value>),
}

impl Value {
    pub fn typ(&self) -> Type {
        match self {
            Value::X(typ) => typ.clone(),
            Value::Z(typ) => typ.clone(),
            Value::Bit(_) => Type::Bit,
            Value::Word(width, _) => Type::Word(*width),
            Value::Ctor(typ, _symbol_id, _values) => typ.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    context: Vec<(Referent, Value)>,
}

impl Context {
    pub fn get(&self, referent: &Referent) -> Value {
        for (referent_, value) in self.context.iter().rev() {
            if referent_ == referent {
                return value.clone();
            }
        }
        panic!("No referent found: {referent:?}")
    }
}

impl Expr {
    pub fn eval(&self, context: Context) -> Value {
        match self.payload() {
            super::expr::ExprPayload::Reference(reference) => context.get(&reference.referent),
            super::expr::ExprPayload::Paren(paren) => paren.subject.eval(context),
            super::expr::ExprPayload::If(_) => todo!(),
            super::expr::ExprPayload::Match(_) => todo!(),
            super::expr::ExprPayload::BitLit(bit_lit) => Value::Bit(bit_lit.value),
            super::expr::ExprPayload::WordLit(word_lit) => Value::Word(word_lit.width, word_lit.value),
            super::expr::ExprPayload::StrLit(str_lit) => todo!(),
            super::expr::ExprPayload::BinOp(bin_op) => todo!(),
            super::expr::ExprPayload::UnOp(un_op) => todo!(),
            super::expr::ExprPayload::Method(method) => todo!(),
            super::expr::ExprPayload::Fn(_) => todo!(),
            super::expr::ExprPayload::Ctor(ctor) => todo!(),
            super::expr::ExprPayload::Enumerant(enumerant) => todo!(),
            super::expr::ExprPayload::Struct(_) => todo!(),
            super::expr::ExprPayload::Index(index) => todo!(),
            super::expr::ExprPayload::IndexRange(index_range) => todo!(),
            super::expr::ExprPayload::Word(word) => todo!(),
            super::expr::ExprPayload::Zext(zext) => todo!(),
            super::expr::ExprPayload::Sext(sext) => todo!(),
            super::expr::ExprPayload::As(as_) => as_.subject.eval(context),
            super::expr::ExprPayload::Hole(hole) => todo!(),
        }
    }
}

#[test]
fn test_eval() {
    let db = crate::util::db_from_dir_with_lib("../examples/passthrough/src", "../lib");
    crate::util::check_db(&db).unwrap();
    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"passthrough::Passthrough".into()).unwrap();
    let elab = db.get_elaboration(top.id());
    dbg!(&elab);
    let inp = elab.resolve(b"top.out").unwrap();
    dbg!(&inp);
    let expr = crate::sim::expr::driver_to_expr(&db, inp.driver().unwrap());
    dbg!(&expr);
}
