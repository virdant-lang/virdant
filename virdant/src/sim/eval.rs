use std::sync::Arc;

use indexmap::IndexMap;

use crate::common;
use crate::common::{Width, WordValue};
use crate::sim::payload;
use crate::types::Type;
use crate::sim::expr::{Expr, ExprPayload, Referent};
use crate::analysis::symbols::SymbolId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    X(Type),
    Z(Type),
    Bit(bool),
    Word(Width, WordValue),
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

/// Per-eval binding environment.
///
/// Holds `Arc<Value>` rather than `Value` so that `Sim::build_context`
/// can populate it with refcount bumps instead of deep-cloning every
/// live signal.  `IndexMap` gives `get` an `O(1)` lookup; insertion
/// order also matches the original `Vec`-based semantics, where a
/// later insert shadowed an earlier one (the original `get` walked
/// `iter().rev()`; `IndexMap::insert` is last-write-wins).
#[derive(Clone, Debug)]
pub struct Context {
    context: IndexMap<Referent, Arc<Value>>,
}

impl Context {
    pub fn new(entries: Vec<(Referent, Arc<Value>)>) -> Context {
        let mut context: IndexMap<Referent, Arc<Value>> =
            IndexMap::with_capacity(entries.len());
        for (r, v) in entries {
            context.insert(r, v);
        }
        Context { context }
    }

    pub fn get(&self, referent: &Referent) -> Value {
        match self.context.get(referent) {
            Some(arc) => arc.as_ref().clone(),
            None => panic!("No referent found: {referent:?}"),
        }
    }

    /// Build a new `Context` with `extra` bindings appended.  Bindings in
    /// `extra` shadow any matching binding already in `self` (matches the
    /// original `iter().rev()` lookup order).  Used by `eval_match` to add
    /// arm-bound pattern variables without mutating the parent context.
    fn extend(&self, extra: Vec<(Referent, Arc<Value>)>) -> Context {
        let mut context = self.context.clone();
        for (r, v) in extra {
            context.insert(r, v);
        }
        Context { context }
    }
}

/// Returns a bitmask selecting the low `width` bits of a u64.
/// For width >= 64 returns u64::MAX.
fn word_mask(width: Width) -> u64 {
    if width >= 64 { u64::MAX } else { (1u64 << (width as u32)).wrapping_sub(1) }
}

/// Structural equality used to evaluate `Eq` / `Neq` binary operators.
fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Bit(l), Value::Bit(r)) => l == r,
        (Value::Word(w, l), Value::Word(_, r)) => {
            (l & word_mask(*w)) == (r & word_mask(*w))
        }
        (Value::Ctor(_, sym_l, args_l), Value::Ctor(_, sym_r, args_r)) => {
            sym_l == sym_r
                && args_l.len() == args_r.len()
                && args_l.iter().zip(args_r.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => false,
    }
}

impl Expr {
    pub fn eval(&self, context: &Context) -> Value {
        match self.payload() {
            ExprPayload::Reference(reference) => context.get(&reference.referent),
            ExprPayload::Paren(paren) => paren.subject.eval(context),
            ExprPayload::If(if_) => self.eval_if(context, if_),
            ExprPayload::Match(match_) => self.eval_match(context, match_),
            ExprPayload::BitLit(bit_lit) => Value::Bit(bit_lit.value),
            ExprPayload::WordLit(word_lit) => Value::Word(word_lit.width, word_lit.value),
            ExprPayload::StrLit(_) => todo!("eval StrLit: string literal evaluation is not supported"),
            ExprPayload::BinOp(binop) => self.eval_binop(context, binop),
            ExprPayload::UnOp(un_op) => self.eval_unop(context, un_op),
            ExprPayload::Method(_) => todo!("eval Method: method evaluation is not implemented"),
            ExprPayload::Fn(_) => todo!(
                "eval Fn: primitive function evaluation requires access to the typing system \
                 (Tag::PrimitiveResolution) to identify which primitive is called; \
                 ExprReference for function names also hits a todo!() in convert_ast_expr"
            ),
            ExprPayload::Ctor(ctor) => self.eval_ctor(context, ctor),
            ExprPayload::Enumerant(enumerant) => self.eval_enumerant(enumerant),
            ExprPayload::Struct(_) => todo!("eval Struct: struct literal evaluation is not implemented"),
            ExprPayload::Index(index) => self.eval_index(context, index),
            ExprPayload::IndexRange(index_range) => self.eval_index_range(context, index_range),
            ExprPayload::Word(word) => self.eval_word(context, word),
            ExprPayload::Zext(zext) => self.eval_zext(context, zext),
            ExprPayload::Sext(sext) => self.eval_sext(context, sext),
            ExprPayload::Cast(cast) => cast.subject.eval(context),
            ExprPayload::Any(any) => self.eval_any(context, any),
            ExprPayload::All(all) => self.eval_all(context, all),
            ExprPayload::As(as_) => as_.subject.eval(context),
            // Hole is an unfilled expression placeholder — treat as X of the appropriate type.
            ExprPayload::Hole(_) => Value::X(self.typ().clone()),
        }
    }

    /// Evaluate an `if`/`else if`/`else` chain.
    /// Matches Verilog ternary semantics: the first truthy branch wins.
    /// X in any condition poisons the whole result.
    fn eval_if(&self, context: &Context, if_: &payload::If) -> Value {
        for (cond, body) in &if_.branches {
            let cond_val = cond.eval(context);
            if cond_val.is_x() {
                return Value::X(self.typ().clone());
            }
            let taken = match cond_val {
                Value::Bit(b) => b,
                Value::Word(_, v) => v != 0,
                _ => unreachable!("if condition must be Bit or Word"),
            };
            if taken {
                return body.eval(context);
            }
        }
        if_.else_branch.eval(context)
    }

    /// Evaluate a `match` expression.
    ///
    /// Matches Verilog `casez` semantics: the first matching arm wins, and the
    /// default arm (`MatchPattern::Else`) is a catch-all.  An unmatched subject
    /// (no arm applies and no else arm) returns X, mirroring the Verilog default
    /// casez arm that drives X.
    ///
    /// Works at the semantic level: `Value::Ctor` (produced by `eval_ctor` /
    /// `eval_enumerant`) is matched by comparing `SymbolId`s rather than
    /// inspecting bit-packed tags.
    fn eval_match(&self, context: &Context, match_: &payload::Match) -> Value {
        let subject = match_.subject.eval(context);
        if subject.is_x() {
            return Value::X(self.typ().clone());
        }
        for (pattern, body) in &match_.arms {
            match (pattern, &subject) {
                // Else arm: always matches
                (payload::Pat::Else, _) => {
                    return body.eval(context);
                }
                // Ctor pattern against Ctor value: compare symbol IDs
                (
                    payload::Pat::Ctor { symbol_id, bound_vars },
                    Value::Ctor(_, subject_sym, subject_args),
                ) => {
                    if subject_sym == symbol_id {
                        let extra: Vec<(Referent, Arc<Value>)> = bound_vars
                            .iter()
                            .zip(subject_args.iter())
                            .map(|((_var_name, var_loc), arg_value)| {
                                (
                                    Referent::Location(var_loc.clone()),
                                    Arc::new(arg_value.clone()),
                                )
                            })
                            .collect();
                        let arm_context = context.extend(extra);
                        return body.eval(&arm_context);
                    }
                }
                // WordLit pattern against Word value: compare numeric values
                (
                    payload::Pat::WordLit { width, value },
                    Value::Word(subject_width, subject_value),
                ) => {
                    let mask = word_mask(*width);
                    if (subject_value & mask) == (value & mask) && subject_width == width {
                        return body.eval(context);
                    }
                }
                // Any other combination is a type error that should have been caught
                _ => unreachable!(
                    "invalid pattern/subject combination: pattern={:?}, subject={:?}",
                    pattern, subject
                ),
            }
        }
        // No arm matched and no else arm — return X (matches Verilog casez default arm).
        Value::X(self.typ().clone())
    }

    /// Evaluate a binary operator.
    ///
    /// Semantics match `convert_binop` in `verilog/conversion.rs`:
    /// - `LogicalAnd/Or/Xor` operate on `Bit` values (1-bit boolean logic).
    /// - `And/Or/Xor` operate on `Word` values (bitwise).
    /// - Comparison operators (`Lt`, `Lte`, `Gt`, `Gte`, `Eq`, `Neq`) return `Bit`.
    /// - `Add` / `Sub` wrap modulo 2^width, matching Verilog unsigned arithmetic.
    /// X in either operand poisons the result to `X` of the **result** type.
    fn eval_binop(&self, context: &Context, binop: &payload::BinOp) -> Value {
        let lhs_val = binop.lhs.eval(context);
        let rhs_val = binop.rhs.eval(context);
        // X poisons the result; use the result type (self.typ()), not operand types.
        if lhs_val.is_x() || rhs_val.is_x() {
            return Value::X(self.typ().clone());
        }
        match binop.op {
            common::BinOp::LogicalAnd => {
                let Value::Bit(l) = lhs_val else { unreachable!() };
                let Value::Bit(r) = rhs_val else { unreachable!() };
                Value::Bit(l & r)
            }
            common::BinOp::LogicalOr => {
                let Value::Bit(l) = lhs_val else { unreachable!() };
                let Value::Bit(r) = rhs_val else { unreachable!() };
                Value::Bit(l | r)
            }
            common::BinOp::LogicalXor => {
                let Value::Bit(l) = lhs_val else { unreachable!() };
                let Value::Bit(r) = rhs_val else { unreachable!() };
                Value::Bit(l ^ r)
            }
            common::BinOp::Lt => {
                let Value::Word(_, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Bit(l < r)
            }
            common::BinOp::Lte => {
                let Value::Word(_, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Bit(l <= r)
            }
            common::BinOp::Gt => {
                let Value::Word(_, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Bit(l > r)
            }
            common::BinOp::Gte => {
                let Value::Word(_, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Bit(l >= r)
            }
            common::BinOp::Eq => Value::Bit(values_equal(&lhs_val, &rhs_val)),
            common::BinOp::Neq => Value::Bit(!values_equal(&lhs_val, &rhs_val)),
            common::BinOp::Add => {
                let Value::Word(width, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Word(width, l.wrapping_add(r) & word_mask(width))
            }
            common::BinOp::Sub => {
                let Value::Word(width, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Word(width, l.wrapping_sub(r) & word_mask(width))
            }
            common::BinOp::And => {
                let Value::Word(width, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Word(width, l & r)
            }
            common::BinOp::Or => {
                let Value::Word(width, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Word(width, l | r)
            }
            common::BinOp::Xor => {
                let Value::Word(width, l) = lhs_val else { unreachable!() };
                let Value::Word(_, r) = rhs_val else { unreachable!() };
                Value::Word(width, l ^ r)
            }
        }
    }

    /// Evaluate a unary operator.
    ///
    /// Semantics match `convert_unop` in `verilog/conversion.rs`:
    /// - `Neg` → arithmetic negation (two's complement, wrapping), matches Verilog unary `-`.
    /// - `Inv` → bitwise NOT (`~`), flips all bits within the word width.
    /// - `Not` → logical NOT (`!`), returns `Bit`; zero maps to true, nonzero to false.
    fn eval_unop(&self, context: &Context, un_op: &payload::UnOp) -> Value {
        let val = un_op.subject.eval(context);
        if val.is_x() {
            return Value::X(self.typ().clone());
        }
        match un_op.op {
            common::UnOp::Neg => {
                let Value::Word(width, v) = val else { unreachable!() };
                Value::Word(width, v.wrapping_neg() & word_mask(width))
            }
            common::UnOp::Inv => {
                match val {
                    Value::Bit(b) => Value::Bit(!b),
                    Value::Word(width, v) => Value::Word(width, (!v) & word_mask(width)),
                    _ => unreachable!(),
                }
            }
            common::UnOp::Not => {
                match val {
                    Value::Bit(b) => Value::Bit(!b),
                    Value::Word(_, v) => Value::Bit(v == 0),
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Evaluate a constructor expression, producing `Value::Ctor`.
    ///
    /// Works at the semantic level — does not bit-pack like the Verilog backend.
    /// X in any argument poisons the whole result.
    fn eval_ctor(&self, context: &Context, ctor: &payload::Ctor) -> Value {
        let mut arg_values = Vec::with_capacity(ctor.args.len());
        for arg in &ctor.args {
            let v = arg.eval(context);
            if v.is_x() {
                return Value::X(self.typ().clone());
            }
            arg_values.push(v);
        }
        Value::Ctor(self.typ().clone(), ctor.symbol_id, arg_values)
    }

    /// Evaluate an enumerant reference.
    ///
    /// Enumerants are represented as zero-argument `Ctor` values keyed by their
    /// `SymbolId`.  This lets `eval_match` identify them by symbol without
    /// needing `Db` access (the numeric value is only needed for bit-level output).
    fn eval_enumerant(&self, enumerant: &payload::Enumerant) -> Value {
        Value::Ctor(self.typ().clone(), enumerant.symbol_id, vec![])
    }

    /// Evaluate a single-bit index expression: `subject[index]`.
    /// Matches Verilog bit-select semantics.
    fn eval_index(&self, context: &Context, index: &payload::Index) -> Value {
        let val = index.subject.eval(context);
        if val.is_x() {
            return Value::X(Type::Bit);
        }
        match val {
            Value::Word(_, v) => Value::Bit(((v >> (index.index as u32)) & 1) != 0),
            Value::Bit(b) => {
                debug_assert_eq!(index.index, 0, "Bit index must be 0");
                Value::Bit(b)
            }
            _ => unreachable!("index subject must be Word or Bit"),
        }
    }

    /// Evaluate a bit-range index expression: `subject[index_hi..index_lo]`.
    ///
    /// `index_hi` is **exclusive** (one past the MSB), matching the source language
    /// semantics.  This is confirmed by `convert_expr` using `index_hi - 1` as the
    /// inclusive upper bound for the Verilog `[hi:lo]` range.
    fn eval_index_range(&self, context: &Context, index_range: &payload::IndexRange) -> Value {
        let lo = index_range.index_lo;
        let hi = index_range.index_hi; // exclusive
        let width = hi - lo;
        let val = index_range.subject.eval(context);
        if val.is_x() {
            return Value::X(Type::Word(width));
        }
        match val {
            Value::Word(_, v) => Value::Word(width, (v >> (lo as u32)) & word_mask(width)),
            _ => unreachable!("index range subject must be Word"),
        }
    }

    /// Evaluate a word-concatenation expression `{a, b, c}`.
    ///
    /// First child is the MSB (leftmost in `{}`), last child is the LSB,
    /// matching Verilog `Concat` semantics and `ExprWord` in `convert_expr`.
    /// X in any part poisons the whole result.
    fn eval_word(&self, context: &Context, word: &payload::Word) -> Value {
        let mut values = Vec::with_capacity(word.args.len());
        for arg in &word.args {
            let v = arg.eval(context);
            if v.is_x() {
                return Value::X(self.typ().clone());
            }
            values.push(v);
        }
        let Type::Word(total_width) = self.typ() else {
            unreachable!("eval_word result must be Word")
        };
        let mut result: u64 = 0;
        for val in &values {
            match val {
                Value::Bit(b) => result = (result << 1) | (*b as u64),
                Value::Word(w, v) => result = (result << w) | (v & word_mask(*w)),
                _ => unreachable!("concat arg must be Bit or Word"),
            }
        }
        Value::Word(*total_width, result & word_mask(*total_width))
    }

    /// Evaluate a zero-extension: `zext(subject)`.
    ///
    /// Matches Verilog `{repeat(extend_by, 1'b0), inner}` from `convert_zext`.
    /// Upper bits are filled with zeros; the low bits are taken from the subject value.
    fn eval_zext(&self, context: &Context, zext: &payload::Zext) -> Value {
        let Type::Word(target_width) = self.typ() else {
            unreachable!("eval_zext result must be Word")
        };
        let val = zext.subject.eval(context);
        if val.is_x() {
            return Value::X(Type::Word(*target_width));
        }
        match val {
            Value::Bit(b) => Value::Word(*target_width, b as u64),
            Value::Word(_, v) => Value::Word(*target_width, v & word_mask(*target_width)),
            _ => unreachable!("zext subject must be Bit or Word"),
        }
    }

    /// Evaluate a sign-extension: `sext(subject)`.
    ///
    /// Matches Verilog `{repeat(extend_by, inner[msb]), inner}` from `convert_sext`.
    /// The MSB of the subject is replicated into all upper bits.
    fn eval_sext(&self, context: &Context, sext: &payload::Sext) -> Value {
        let Type::Word(target_width) = self.typ() else {
            unreachable!("eval_sext result must be Word")
        };
        let val = sext.subject.eval(context);
        if val.is_x() {
            return Value::X(Type::Word(*target_width));
        }
        match val {
            Value::Bit(b) => {
                // Single-bit sext: replicate the bit into all target positions.
                // Matches Verilog: {extend_by{bit}, bit}
                if b { Value::Word(*target_width, word_mask(*target_width)) }
                else { Value::Word(*target_width, 0) }
            }
            Value::Word(src_width, v) => {
                // Check the MSB of the source.
                // Matches Verilog: {extend_by{src[src_width-1]}, src}
                let msb = (v >> ((src_width - 1) as u32)) & 1;
                if msb == 1 {
                    // Fill all bits above src_width with 1s.
                    let fill = word_mask(*target_width) ^ word_mask(src_width);
                    Value::Word(*target_width, v | fill)
                } else {
                    Value::Word(*target_width, v)
                }
            }
            _ => unreachable!("sext subject must be Bit or Word"),
        }
    }

    fn eval_any(&self, context: &Context, any: &payload::Any) -> Value {
        let subject_value = any.subject.eval(context);
        match subject_value {
            Value::Word(_, val) => Value::Bit(val != 0),
            Value::Bit(b) => Value::Bit(b),
            Value::X(_) | Value::Z(_) => Value::X(Type::Bit),
            Value::Ctor(_, _, _) => Value::X(Type::Bit), // Not applicable for constructors
        }
    }

    fn eval_all(&self, context: &Context, all: &payload::All) -> Value {
        let subject_value = all.subject.eval(context);
        match subject_value {
            Value::Word(width, val) => {
                let all_ones = (1u64 << width) - 1;
                Value::Bit(val == all_ones)
            }
            Value::Bit(b) => Value::Bit(b),
            Value::X(_) | Value::Z(_) => Value::X(Type::Bit),
            Value::Ctor(_, _, _) => Value::X(Type::Bit), // Not applicable for constructors
        }
    }
}

#[test]
fn test_eval() {
    use bstr::BStr;
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
