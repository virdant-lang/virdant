//! Evaluation of simulator `Expr` trees: defines `Value` (a bit pattern
//! tagged with its type), the per-eval binding `Context`, and
//! `Expr::eval` implementing `when`, `match` (casez), binary/unary
//! operators, constructors, indexing, concatenation, and width
//! operations on bit patterns.
//!
//! There are no X or Z values: every value is a fully-defined bit
//! pattern.  Uninitialized cells start from the sim's `InitStrategy`
//! (random by default); a pattern that is not a valid inhabitant of its
//! type (e.g. an out-of-range union tag) simply fails to decode, and
//! prints as `<invalid>`.

use std::sync::Arc;

use indexmap::IndexMap;

use crate::common::{BinOp, UnOp, Width};
use crate::sim::bits::BitPat;
use crate::sim::layout::TypeLayoutIndex;
use crate::sim::payload;
use crate::types::Type;
use crate::sim::expr::{Expr, ExprPayload, Referent};
use crate::analysis::elaboration::SignalId;

/// A simulated value: a bit pattern tagged with its (semantic) type.
///
/// The `typ` tag says how to interpret the bits (see `sim::layout`).
/// Gate logic decodes the bits on demand; aggregate types (unions,
/// enums, structs) are stored pre-encoded.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Value {
    typ: Type,
    bits: BitPat,
}

impl std::fmt::Display for Value {
    /// Print according to the object type: a `Bit` pattern "0" prints
    /// as `false`, a `Word` prints numerically, an aggregate type prints
    /// its raw bit pattern (rich typed rendering — including
    /// `<invalid>` for patterns that are not inhabitants of their type —
    /// is [`TypeLayoutIndex::display`]).
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.typ {
            Type::Bit => f.write_str(if self.bits.get_bit(0) { "true" } else { "false" }),
            Type::Clock | Type::Reset => f.write_str(&self.bits.to_bit_string()),
            Type::Word(w) => {
                if *w < 64 {
                    write!(f, "{}", self.bits.to_u64())
                } else {
                    write!(f, "0b{}", self.bits.to_bit_string())
                }
            }
            Type::Usual(..) => f.write_str(&self.bits.to_bit_string()),
            Type::Param(_) => f.write_str("<invalid>"),
        }
    }
}

impl Value {
    pub fn new(typ: Type, bits: BitPat) -> Value {
        Value { typ, bits }
    }

    /// A single-bit value of `typ` (for `Bit`/`Clock`/`Reset`-typed
    /// signals).
    pub fn from_bool(typ: Type, b: bool) -> Value {
        Value { typ, bits: BitPat::bit(b) }
    }

    pub fn typ(&self) -> &Type {
        &self.typ
    }

    pub fn bits(&self) -> &BitPat {
        &self.bits
    }

    pub fn width(&self) -> Width {
        self.bits.width()
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
    pub(crate) layouts: TypeLayoutIndex,
    context: IndexMap<Referent, Arc<Value>>,
}

impl Context {
    pub fn new(entries: Vec<(Referent, Arc<Value>)>, layouts: TypeLayoutIndex) -> Context {
        let mut context: IndexMap<Referent, Arc<Value>> =
            IndexMap::with_capacity(entries.len());
        for (r, v) in entries {
            context.insert(r, v);
        }
        Context { layouts, context }
    }

    pub fn get(&self, referent: &Referent) -> Value {
        match self.context.get(referent) {
            Some(arc) => arc.as_ref().clone(),
            None => Value::new(Type::Bit, BitPat::bit(false)),
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
        Context { layouts: self.layouts.clone(), context }
    }

    /// Build an eval `Context` for `signal_id`'s driver expression.
    ///
    /// `Circuit::resolved_referents` holds the precomputed `(Referent,
    /// SignalId)` list for every component with a driver — built once in
    /// `Circuit::new` instead of re-walking the expression on every propagation.
    /// This method zips each resolved id with the live `Arc<Value>` from
    /// `state.vals`; the binding clones are refcount bumps, not deep `Value`
    /// copies.
    pub(super) fn context_for_component(
        circuit: &crate::sim::circuit::Circuit,
        state: &crate::sim::state::State,
        signal_id: SignalId,
    ) -> Context {
        let entries = circuit
            .referents(signal_id)
            .iter()
            .map(|(r, id)| (r.clone(), Arc::new(state.get(*id))))
            .collect();
        Context::new(entries, circuit.type_layout_index.clone())
    }
}

impl Expr {
    /// A `Value` of this expression's static type carrying `bits`.
    fn val(&self, bits: BitPat) -> Value {
        Value::new(self.typ().clone(), bits)
    }

    fn layouts(context: &Context) -> &TypeLayoutIndex {
        &context.layouts
    }

    /// The bit width of this expression's static type.
    fn width_of_typ(&self, context: &Context) -> Width {
        context.layouts.type_width(self.typ())
    }

    pub fn eval(&self, context: &Context) -> Value {
        match self.payload() {
            ExprPayload::Reference(reference) => {
                let value = context.get(&reference.referent);
                if value.width() != Self::layouts(context).type_width(self.typ()) {
                    eprintln!("DEBUG ref width mismatch: referent={:?} got_width={} node_typ={:?}", reference.referent, value.width(), self.typ());
                }
                self.val(value.bits().clone())
            }
            ExprPayload::Paren(paren) => paren.subject.eval(context),
            ExprPayload::When(when) => self.eval_when(context, when),
            ExprPayload::Match(match_) => self.eval_match(context, match_),
            ExprPayload::BitLit(bit_lit) => self.val(BitPat::bit(bit_lit.value)),
            ExprPayload::WordLit(word_lit) => {
                self.val(BitPat::from_u64(word_lit.width, word_lit.value))
            }
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
            ExprPayload::Enumerant(enumerant) => self.eval_enumerant(context, enumerant),
            ExprPayload::Struct(struct_) => self.eval_struct(context, struct_),
            ExprPayload::Index(index) => self.eval_index(context, index),
            ExprPayload::IndexDyn(index_dyn) => self.eval_index_dyn(context, index_dyn),
            ExprPayload::IndexRange(index_range) => self.eval_index_range(context, index_range),
            ExprPayload::Word(word) => self.eval_word(context, word),
            ExprPayload::Zext(zext) => self.eval_zext(context, zext),
            ExprPayload::Sext(sext) => self.eval_sext(context, sext),
            ExprPayload::AsClock(asclock) => self.eval_passthrough(context, &asclock.subject),
            ExprPayload::Cast(cast) => self.eval_passthrough(context, &cast.subject),
            ExprPayload::Trunc(trunc) => self.eval_trunc(context, trunc),
            ExprPayload::Any(any) => self.eval_any(context, any),
            ExprPayload::All(all) => self.eval_all(context, all),
            ExprPayload::As(as_) => self.eval_passthrough(context, &as_.subject),
            // Hole is an unfilled expression placeholder — its bits are arbitrary.
            ExprPayload::Hole(_) => self.val(BitPat::zeros(self.width_of_typ(context))),
            // Dontcare is a valid value the compiler is free to choose.
            ExprPayload::Dontcare(_) => self.val(BitPat::zeros(self.width_of_typ(context))),
        }
    }

    /// Evaluate an expression whose value passes the subject's bits
    /// through unchanged, re-tagged with this node's type (`as`,
    /// `cast`, `asclock`).
    fn eval_passthrough(&self, context: &Context, subject: &Expr) -> Value {
        let subject_val = subject.eval(context);
        self.val(subject_val.bits().clone())
    }

    /// Truthiness of a condition value: a `Bit` is true when its bit is
    /// set; a `Word` is true when non-zero.
    fn truthy(value: &Value) -> bool {
        if value.width() == 1 {
            value.bits().get_bit(0)
        } else {
            !value.bits().is_zero()
        }
    }

    /// Evaluate a `when` chain.
    /// The first truthy branch wins; an untruthy condition falls through
    /// to the next branch (Verilog ternary semantics without X: every
    /// condition is a fully-defined bit pattern).
    fn eval_when(&self, context: &Context, when: &payload::When) -> Value {
        for (cond, body) in &when.branches {
            let cond_val = cond.eval(context);
            if Self::truthy(&cond_val) {
                return body.eval(context);
            }
        }
        when.else_branch.eval(context)
    }

    /// Evaluate a `match` expression.
    ///
    /// Matches Verilog `casez` semantics: the first matching arm wins, and the
    /// default arm (`MatchPattern::Else`) is a catch-all.  An unmatched subject
    /// (no arm applies and no else arm) yields a zero bit pattern of the
    /// result type — which, for constructor types, is simply an invalid
    /// value.
    ///
    /// Constructor patterns decode the subject's bits through the type
    /// layout: the tag selects the slot, the payload bits are bound to
    /// the pattern's variables.  A subject whose bits do not decode
    /// (out-of-range tag) matches no arm.
    fn eval_match(&self, context: &Context, match_: &payload::Match) -> Value {
        let subject = match_.subject.eval(context);
        let layouts = Self::layouts(context);
        for (pattern, body) in &match_.arms {
            match pattern {
                // Else arm: always matches
                payload::Pat::Else => {
                    return body.eval(context);
                }
                // Ctor pattern: decode the subject's tag/payload through
                // the layout and bind the payload to the pattern variables.
                payload::Pat::Ctor { symbol_id, bound_vars } => {
                    let Some((slot, payloads)) = layouts.decode_ctor(subject.typ(), subject.bits())
                    else {
                        continue;
                    };
                    if slot.symbol_id != *symbol_id {
                        continue;
                    }
                    let extra: Vec<(Referent, Arc<Value>)> = bound_vars
                        .iter()
                        .zip(payloads.iter())
                        .zip(slot.param_types.iter())
                        .map(|(((_var_name, var_loc), bits), typ)| {
                            (
                                Referent::Location(var_loc.clone()),
                                Arc::new(Value::new(typ.clone(), bits.clone())),
                            )
                        })
                        .collect();
                    let arm_context = context.extend(extra);
                    return body.eval(&arm_context);
                }
                // WordLit pattern: compare bit patterns.
                payload::Pat::WordLit { width, value } => {
                    let expected = BitPat::from_u64(*width, *value);
                    if subject.width() == *width && subject.bits() == &expected {
                        return body.eval(context);
                    }
                }
                // BitLit pattern: compare bit patterns.
                payload::Pat::BitLit { value } => {
                    if subject.width() == 1 && subject.bits().get_bit(0) == *value {
                        return body.eval(context);
                    }
                }
            }
        }
        // No arm matched and no else arm — zero bits of the result type.
        self.val(BitPat::zeros(self.width_of_typ(context)))
    }

    /// Evaluate a binary operator.
    ///
    /// Semantics match `convert_binop` in `verilog/conversion.rs`:
    /// - `LogicalAnd/Or/Xor` operate on `Bit` values (1-bit boolean logic).
    /// - `And/Or/Xor` operate on `Word` values (bitwise).
    /// - Comparison operators (`Lt`, `Lte`, `Gt`, `Gte`, `Eq`, `Neq`) return `Bit`.
    /// - `Add` / `Sub` wrap modulo 2^width, matching Verilog unsigned arithmetic.
    fn eval_binop(&self, context: &Context, binop: &payload::BinOp) -> Value {
        let lhs_val = binop.lhs.eval(context);
        let rhs_val = binop.rhs.eval(context);
        let lhs = lhs_val.bits();
        let rhs = rhs_val.bits();
        match binop.op {
            BinOp::LogicalAnd => self.val(BitPat::bit(lhs.get_bit(0) && rhs.get_bit(0))),
            BinOp::LogicalOr => self.val(BitPat::bit(lhs.get_bit(0) || rhs.get_bit(0))),
            BinOp::LogicalXor => self.val(BitPat::bit(lhs.get_bit(0) ^ rhs.get_bit(0))),
            BinOp::Lt => self.val(BitPat::bit(lhs.ucmp(rhs) == std::cmp::Ordering::Less)),
            BinOp::Lte => self.val(BitPat::bit(lhs.ucmp(rhs) != std::cmp::Ordering::Greater)),
            BinOp::Gt => self.val(BitPat::bit(lhs.ucmp(rhs) == std::cmp::Ordering::Greater)),
            BinOp::Gte => self.val(BitPat::bit(lhs.ucmp(rhs) != std::cmp::Ordering::Less)),
            BinOp::Eq => self.val(BitPat::bit(lhs == rhs)),
            BinOp::Neq => self.val(BitPat::bit(lhs != rhs)),
            BinOp::Add => self.val(lhs.add(rhs)),
            BinOp::Sub => self.val(lhs.sub(rhs)),
            BinOp::And => self.val(lhs.and(rhs)),
            BinOp::Or => self.val(lhs.or(rhs)),
            BinOp::Xor => self.val(lhs.xor(rhs)),
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
        let bits = val.bits();
        match un_op.op {
            UnOp::Neg => self.val(bits.neg()),
            UnOp::Inv => self.val(bits.not()),
            UnOp::Not => self.val(BitPat::bit(bits.is_zero())),
        }
    }

    /// Evaluate a constructor expression: encode the tag and the
    /// encoded parameters through the type layout.
    fn eval_ctor(&self, context: &Context, ctor: &payload::Ctor) -> Value {
        let arg_values: Vec<BitPat> = ctor
            .args
            .iter()
            .map(|arg| arg.eval(context).bits().clone())
            .collect();
        match Self::layouts(context).encode_ctor(self.typ(), ctor.symbol_id, &arg_values) {
            Some(bits) => self.val(bits),
            None => self.val(BitPat::zeros(self.width_of_typ(context))),
        }
    }

    /// Evaluate an enumerant reference: an enum's tag-only ctor value.
    fn eval_enumerant(&self, context: &Context, enumerant: &payload::Enumerant) -> Value {
        match Self::layouts(context).encode_ctor(self.typ(), enumerant.symbol_id, &[]) {
            Some(bits) => self.val(bits),
            None => self.val(BitPat::zeros(self.width_of_typ(context))),
        }
    }

    /// Evaluate a struct literal: concatenate the fields' bits, first
    /// field at the MSB end (struct layouts carry no tag).
    fn eval_struct(&self, context: &Context, struct_: &payload::Struct) -> Value {
        let field_values: Vec<BitPat> = struct_
            .fields
            .iter()
            .map(|(_, field)| field.eval(context).bits().clone())
            .collect();
        let mut bits = BitPat::zeros(0);
        for value in field_values {
            bits = bits.concat_msb_first(&value);
        }
        self.val(bits.zext_to(self.width_of_typ(context)))
    }

    /// Evaluate a single-bit index expression: `subject[index]`.
    fn eval_index(&self, context: &Context, index: &payload::Index) -> Value {
        let val = index.subject.eval(context);
        self.val(BitPat::bit(val.bits().get_bit(index.index as usize)))
    }

    fn eval_index_dyn(&self, context: &Context, index_dyn: &payload::IndexDyn) -> Value {
        let val = index_dyn.subject.eval(context);
        let idx_val = index_dyn.index.eval(context);
        self.val(BitPat::bit(val.bits().get_bit(idx_val.bits().to_u64() as usize)))
    }

    /// Evaluate a bit-range index expression: `subject[index_hi..index_lo]`.
    ///
    /// `index_hi` is **exclusive** (one past the MSB), matching the source language
    /// semantics.  This is confirmed by `convert_expr` using `index_hi - 1` as the
    /// inclusive upper bound for the Verilog `[hi:lo]` range.
    fn eval_index_range(&self, context: &Context, index_range: &payload::IndexRange) -> Value {
        let lo = index_range.index_lo;
        let hi = index_range.index_hi; // exclusive
        let val = index_range.subject.eval(context);
        if hi as usize > val.bits().width() as usize {
            eprintln!("DEBUG index_range: lo={lo} hi={hi} subject_width={} subject_typ={:?} subject_bits_width={}", val.bits().width(), val.typ(), val.bits().width());
        }
        self.val(val.bits().slice(lo, hi))
    }

    /// Evaluate a word-concatenation expression `{a, b, c}`.
    ///
    /// First child is the MSB (leftmost in `{}`), last child is the LSB,
    /// matching Verilog `Concat` semantics and `ExprWord` in `convert_expr`.
    fn eval_word(&self, context: &Context, word: &payload::Word) -> Value {
        let mut bits = BitPat::zeros(0);
        for arg in &word.args {
            let v = arg.eval(context);
            bits = bits.concat_msb_first(v.bits());
        }
        self.val(bits.zext_to(self.width_of_typ(context)))
    }

    /// Evaluate a zero-extension: `zext(subject)`.
    ///
    /// Matches Verilog `{repeat(extend_by, 1'b0), inner}` from `convert_zext`.
    /// Upper bits are filled with zeros; the low bits are taken from the subject value.
    fn eval_zext(&self, context: &Context, zext: &payload::Zext) -> Value {
        let val = zext.subject.eval(context);
        self.val(val.bits().zext_to(self.width_of_typ(context)))
    }

    /// Evaluate a sign-extension: `sext(subject)`.
    ///
    /// Matches Verilog `{repeat(extend_by, inner[msb]), inner}` from `convert_sext`.
    /// The MSB of the subject is replicated into all upper bits.
    fn eval_sext(&self, context: &Context, sext: &payload::Sext) -> Value {
        let val = sext.subject.eval(context);
        self.val(val.bits().sext_to(self.width_of_typ(context)))
    }

    /// Evaluate a truncation: `trunc(subject)`.
    ///
    /// The low `target_width` bits are taken from the subject value.
    /// If subject is wider than target, the upper bits are discarded.
    fn eval_trunc(&self, context: &Context, trunc: &payload::Trunc) -> Value {
        let val = trunc.subject.eval(context);
        self.val(val.bits().zext_to(self.width_of_typ(context)))
    }

    fn eval_any(&self, context: &Context, any: &payload::Any) -> Value {
        let subject_value = any.subject.eval(context);
        self.val(BitPat::bit(!subject_value.bits().is_zero()))
    }

    fn eval_all(&self, context: &Context, all: &payload::All) -> Value {
        let subject_value = all.subject.eval(context);
        self.val(BitPat::bit(subject_value.bits().is_all_ones()))
    }
}


