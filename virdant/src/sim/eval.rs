use bstr::BStr;
use indexmap::IndexMap;

use crate::common::Width;
use crate::sim::{ExprPayload, payload};
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

    pub fn is_x(&self) -> bool {
        matches!(self, Value::X(_))
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    context: Vec<(Referent, Value)>,
}

impl Context {
    pub fn new(entries: Vec<(Referent, Value)>) -> Context {
        Context { context: entries }
    }

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
        let result = match self.payload() {
            ExprPayload::Reference(reference) => context.get(&reference.referent),
            ExprPayload::Paren(paren) => paren.subject.eval(context),
            ExprPayload::If(_) => todo!(),
            ExprPayload::Match(_) => todo!(),
            ExprPayload::BitLit(bit_lit) => Value::Bit(bit_lit.value),
            ExprPayload::WordLit(word_lit) => Value::Word(word_lit.width, word_lit.value),
            ExprPayload::StrLit(str_lit) => todo!(),
            ExprPayload::BinOp(binop) => self.eval_binop(context, binop),
            ExprPayload::UnOp(un_op) => todo!(),
            ExprPayload::Method(method) => todo!(),
            ExprPayload::Fn(_) => todo!(),
            ExprPayload::Ctor(ctor) => todo!(),
            ExprPayload::Enumerant(enumerant) => todo!(),
            ExprPayload::Struct(_) => todo!(),
            ExprPayload::Index(index) => todo!(),
            ExprPayload::IndexRange(index_range) => todo!(),
            ExprPayload::Word(word) => todo!(),
            ExprPayload::Zext(zext) => todo!(),
            ExprPayload::Sext(sext) => todo!(),
            ExprPayload::As(as_) => as_.subject.eval(context),
            ExprPayload::Hole(hole) => todo!(),
        };

        result
    }

    fn eval_binop(&self, context: Context, binop: &payload::BinOp) -> Value {
        let lhs_val = binop.lhs.eval(context.clone());
        let rhs_val = binop.rhs.eval(context);
        if lhs_val.is_x() { return Value::X(binop.lhs.typ().clone()); }
        if rhs_val.is_x() { return Value::X(binop.rhs.typ().clone()); }

        match binop.op {
            crate::common::BinOp::LogicalAnd => todo!(),
            crate::common::BinOp::LogicalOr => todo!(),
            crate::common::BinOp::LogicalXor => todo!(),
            crate::common::BinOp::Lt => todo!(),
            crate::common::BinOp::Lte => todo!(),
            crate::common::BinOp::Gt => todo!(),
            crate::common::BinOp::Gte => todo!(),
            crate::common::BinOp::Eq => todo!(),
            crate::common::BinOp::Neq => todo!(),
            crate::common::BinOp::Add => {
                let Value::Word(width0,  val0) = lhs_val else { unreachable!() };
                let Value::Word(_width1, val1) = rhs_val else { unreachable!() };
                Value::Word(width0, (val0 + val1) % (1 << width0) as u64)
            }
            crate::common::BinOp::Sub => todo!(),
            crate::common::BinOp::And => todo!(),
            crate::common::BinOp::Or => todo!(),
            crate::common::BinOp::Xor => todo!(),
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
    let inp = elab.resolve(BStr::new(b"top.out")).unwrap();
    dbg!(&inp);
    let expr = crate::sim::expr::driver_to_expr(&db, inp.driver().unwrap());
    dbg!(&expr);
}
