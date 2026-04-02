use hashbrown::HashMap;

use crate::common::Width;
use crate::types::Type;
use crate::sim::expr::{Expr, Referent};
use crate::analysis::symbols::SymbolId;

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

pub struct Context {
    context: HashMap<Referent, Value>,
}

impl Expr {
    pub fn eval(&self, context: Context) -> Value {
        match self.payload() {
            super::expr::ExprPayload::Reference(reference) => todo!(),
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
