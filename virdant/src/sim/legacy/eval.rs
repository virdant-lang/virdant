//! Evaluation of simulator `Expr` trees: the per-eval binding
//! `Context`, and `Expr::eval` implementing `when`, `match` (casez),
//! binary/unary operators, constructors, indexing, concatenation, and
//! width operations on bit patterns.
//!
//! This is the legacy engine's interpreter (`sim::legacy::eval`);
//! `Value` itself lives in `sim::value`, shared with the engines and
//! the public `Sim` API.
//!
//! There are no X or Z values: every value is a fully-defined bit
//! pattern.  Uninitialized cells start from the sim's `InitStrategy`
//! (random by default); a pattern that is not a valid inhabitant of its
//! type (e.g. an out-of-range union tag) simply fails to decode, and
//! prints as `<invalid>`.

use indexmap::IndexMap;

use crate::common::{BinOp, UnOp, Width};
use crate::sim::bits::BitPat;
use crate::sim::layout::TypeLayoutIndex;
use crate::sim::payload;
use crate::types::Type;
use crate::sim::expr::{Expr, ExprPayload, Referent};
use crate::sim::legacy::state::State;
use crate::sim::value::{mask, Value, InternalValue};
use crate::analysis::elaboration::SignalId;


/// Add-with-carry / subtract-with-borrow on same-width patterns:
/// zero-extend both operands by one bit, then add or subtract.  The
/// extra most-significant bit of the result holds the carry-out
/// (`AddCarry`) or borrow-out (`SubBorrow`).
fn widened_addsub(op: BinOp, lhs: &BitPat, rhs: &BitPat) -> BitPat {
    assert!(lhs.width() == rhs.width(), "widened_addsub: width mismatch");
    let w = lhs.width();
    let mut a = BitPat::zeros(w + 1);
    let mut b = BitPat::zeros(w + 1);
    for i in 0..w as usize {
        a.set_bit(i, lhs.get_bit(i));
        b.set_bit(i, rhs.get_bit(i));
    }
    match op {
        BinOp::AddCarry => a.add(&b),
        BinOp::SubBorrow => a.sub(&b),
        _ => unreachable!("widened_addsub: not a carry/borrow op"),
    }
}

/// Per-eval binding environment.
///
/// Holds plain `Value`s: with interned type tags, a `Value` binding is a
/// small inline struct for the common (≤64-bit) case, so copying live
/// signal values into the context is a memcpy, not an allocation.
/// `IndexMap` gives `get` an `O(1)` lookup; insertion order also matches
/// the original `Vec`-based semantics, where a later insert shadowed an
/// earlier one (the original `get` walked `iter().rev()`;
/// `IndexMap::insert` is last-write-wins).
#[derive(Clone, Debug)]
pub struct Context {
    pub(crate) layouts: TypeLayoutIndex,
    context: IndexMap<Referent, Value>,
    /// Memory words keyed by the memory's referent, for `mem[dyn i]` /
    /// `mem[c]` reads.  Cheap `Arc` clones of the
    /// live arrays, snapshotted at context build time.
    memories: IndexMap<Referent, std::sync::Arc<Vec<Value>>>,
}

impl Context {
    pub fn new(entries: Vec<(Referent, Value)>, layouts: TypeLayoutIndex) -> Context {
        let mut context: IndexMap<Referent, Value> =
            IndexMap::with_capacity(entries.len());
        for (r, v) in entries {
            context.insert(r, v);
        }
        Context { layouts, context, memories: IndexMap::new() }
    }

    /// The words of the memory referenced by `referent`, if it is one
    /// (a read of `mem[...]` consults these; the memory's scalar
    /// `Context` value is only a change-detection version counter).
    pub(crate) fn memory_words(&self, referent: &Referent) -> Option<&std::sync::Arc<Vec<Value>>> {
        self.memories.get(referent)
    }

    /// Bind a memory's words to its referent (used when building an
    /// evaluation context for a component whose expressions read the
    /// memory).
    pub(crate) fn insert_memory(&mut self, referent: Referent, words: std::sync::Arc<Vec<Value>>) {
        self.memories.insert(referent, words);
    }

    pub fn get(&self, referent: &Referent) -> Value {
        match self.context.get(referent) {
            Some(value) => value.clone(),
            None => Value::new(self.layouts.type_id(&Type::Bit), BitPat::bit(false)),
        }
    }

    /// Build a new `Context` with `extra` bindings appended.  Bindings in
    /// `extra` shadow any matching binding already in `self` (matches the
    /// original `iter().rev()` lookup order).  Used by `eval_match` to add
    /// arm-bound pattern variables without mutating the parent context.
    fn extend(&self, extra: Vec<(Referent, Value)>) -> Context {
        let mut context = self.context.clone();
        for (r, v) in extra {
            context.insert(r, v);
        }
        Context { layouts: self.layouts.clone(), context, memories: self.memories.clone() }
    }

    /// Build an eval `Context` for `signal_id`'s driver expression.
    ///
    /// `Circuit::resolved_referents` holds the precomputed `(Referent,
    /// SignalId)` list for every component with a driver — built once in
    /// `Circuit::new` instead of re-walking the expression on every propagation.
    /// This method zips each resolved id with the live value from
    /// `state.vals`; the binding copies are cheap inline copies for
    /// small values.
    pub(super) fn context_for_component(
        circuit: &crate::sim::circuit::Circuit,
        state: &State,
        signal_id: SignalId,
    ) -> Context {
        let entries = circuit
            .referents(signal_id)
            .iter()
            .map(|(r, id)| (r.clone(), state.get(*id)))
            .collect();
        let mut context = Context::new(entries, circuit.type_layout_index.clone());
        // Memory reads fetch the words, not the version counter.
        for (r, id) in circuit.referents(signal_id) {
            if circuit.is_memory(*id) {
                context.insert_memory(r.clone(), state.memory_words(*id));
            }
        }
        context
    }
}

impl Expr {
    /// A `Value` of this expression's static type carrying `bits`.
    fn val(&self, bits: BitPat) -> Value {
        Value::new(self.typ_id(), bits)
    }

    /// A single-bit `Value` of this expression's static type —
    /// allocation-free (`Value::from_bool` never materialises a `BitPat`).
    fn val_bool(&self, b: bool) -> Value {
        Value::from_bool(self.typ_id(), b)
    }

    /// A zero pattern of `width` bits of this expression's static type —
    /// allocation-free for `width <= 64`; `Value::from_u64` falls back to
    /// the `Big` representation for wider types.
    fn val_zeros(&self, width: Width) -> Value {
        Value::from_u64(self.typ_id(), width, 0)
    }

    /// A `Value` of this expression's static type carrying `value`'s
    /// bits (re-tags `value`, keeping its representation).
    fn val_value(&self, value: Value) -> Value {
        value.with_typ(self.typ_id())
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
                self.val_value(value)
            }
            ExprPayload::Paren(paren) => paren.subject.eval(context),
            ExprPayload::If(when) => self.eval_when(context, when),
            ExprPayload::Match(match_) => self.eval_match(context, match_),
            ExprPayload::BitLit(bit_lit) => self.val_bool(bit_lit.value),
            ExprPayload::WordLit(word_lit) => {
                Value::from_u64(self.typ_id(), word_lit.width, word_lit.value)
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
            ExprPayload::Hole(_) => self.val_zeros(self.width_of_typ(context)),
            // Dontcare is a valid value the compiler is free to choose.
            ExprPayload::Dontcare(_) => self.val_zeros(self.width_of_typ(context)),
        }
    }

    /// Evaluate an expression whose value passes the subject's bits
    /// through unchanged, re-tagged with this node's type (`as`,
    /// `cast`, `asclock`).
    fn eval_passthrough(&self, context: &Context, subject: &Expr) -> Value {
        let subject_val = subject.eval(context);
        self.val_value(subject_val)
    }

    /// Truthiness of a condition value: a `Bit` is true when its bit is
    /// set; a `Word` is true when non-zero.
    fn truthy(value: &Value) -> bool {
        if value.width() == 1 {
            value.get_bit(0)
        } else {
            !value.is_zero()
        }
    }

    /// Evaluate a `when` chain.
    /// The first truthy branch wins; an untruthy condition falls through
    /// to the next branch (Verilog ternary semantics without X: every
    /// condition is a fully-defined bit pattern).
    fn eval_when(&self, context: &Context, when: &payload::If) -> Value {
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
    /// Ctor pattern: decode the subject's tag/payload through the
    /// layout, require every sub-pattern to match its payload, and
    /// bind the matched values to the pattern's variables (at any
    /// nesting depth).
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
                // the layout and match each sub-pattern against its
                // payload, binding ident sub-patterns.
                payload::Pat::Ctor { symbol_id, sub_pats, .. } => {
                    let Some((slot, payloads)) =
                        layouts.decode_ctor(subject.typ_id(), &subject.bitpat())
                    else {
                        continue;
                    };
                    if slot.symbol_id != *symbol_id {
                        continue;
                    }
                    let mut bindings: Vec<(Referent, Value)> = Vec::new();
                    let matched = sub_pats.iter()
                        .zip(payloads.iter())
                        .zip(slot.param_types.iter())
                        .all(|((pat, bits), typ)| {
                            let payload_value = Value::new(layouts.type_id(typ), bits.clone());
                            Self::pat_matches(pat, &payload_value, layouts, &mut bindings)
                        });
                    if !matched {
                        continue;
                    }
                    let arm_context = context.extend(bindings);
                    return body.eval(&arm_context);
                }
                // WordLit pattern: compare bit patterns.
                payload::Pat::WordLit { width, value } => {
                    let expected = BitPat::from_u64(*width, *value);
                    if subject.width() == *width && subject.bits_eq(&expected) {
                        return body.eval(context);
                    }
                }
                // BitLit pattern: compare bit patterns.
                payload::Pat::BitLit { value } => {
                    if subject.width() == 1 && subject.get_bit(0) == *value {
                        return body.eval(context);
                    }
                }
                // Bind and Wild arms only occur as sub-patterns, never
                // at the top level of a match.
                payload::Pat::Bind { .. } | payload::Pat::Wild => {}
            }
        }
        // No arm matched and no else arm — zero bits of the result type.
        self.val_zeros(self.width_of_typ(context))
    }

    /// Does `value` match the sub-`pat`?  Ident sub-patterns bind
    /// `value` into `bindings` (at any nesting depth); nested ctor
    /// sub-patterns decode `value` through the type layout and match
    /// their own sub-patterns recursively.
    fn pat_matches(
        pat: &payload::Pat,
        value: &Value,
        layouts: &TypeLayoutIndex,
        bindings: &mut Vec<(Referent, Value)>,
    ) -> bool {
        match pat {
            payload::Pat::Bind { location } => {
                bindings.push((Referent::Location(location.clone()), value.clone()));
                true
            }
            payload::Pat::Wild | payload::Pat::Else => true,
            payload::Pat::WordLit { width, value: expected } => {
                value.width() == *width && value.bits_eq(&BitPat::from_u64(*width, *expected))
            }
            payload::Pat::BitLit { value: expected } => {
                value.width() == 1 && value.get_bit(0) == *expected
            }
            payload::Pat::Ctor { symbol_id, sub_pats, .. } => {
                let Some((slot, payloads)) = layouts.decode_ctor(value.typ_id(), &value.bitpat())
                else {
                    return false;
                };
                if slot.symbol_id != *symbol_id {
                    return false;
                }
                sub_pats.iter()
                    .zip(payloads.iter())
                    .zip(slot.param_types.iter())
                    .all(|((sub_pat, bits), typ)| {
                        let payload_value = Value::new(layouts.type_id(typ), bits.clone());
                        Self::pat_matches(sub_pat, &payload_value, layouts, bindings)
                    })
            }
        }
    }

    /// Evaluate a binary operator.
    ///
    /// Semantics match `convert_binop` in `verilog/conversion.rs`:
    /// - `LogicalAnd/Or/Xor` operate on `Bit` values (1-bit boolean logic).
    /// - `And/Or/Xor` operate on `Word` values (bitwise).
    /// - Comparison operators (`Lt`, `Lte`, `Gt`, `Gte`, `Eq`, `Neq`) return `Bit`.
    /// - `Add` / `Sub` wrap modulo 2^width, matching Verilog unsigned arithmetic.
    /// - `AddCarry` / `SubBorrow` return a result one bit wider than their
    ///   operands: the extra MSB is the carry-out / borrow-out.
    fn eval_binop(&self, context: &Context, binop: &payload::BinOp) -> Value {
        let lhs_val = binop.lhs.eval(context);
        let rhs_val = binop.rhs.eval(context);

        // Hot path: both operands fit in a `u64` — compute in machine
        // words, no `BitPat` allocation.  Carry/borrow results are one
        // bit wider than their operands, so 64-bit operands fall back
        // to the `BitPat` path below.
        if let (
            InternalValue::Small { width: lw, bits: a, .. },
            InternalValue::Small { width: rw, bits: b, .. },
        ) = (&lhs_val.0, &rhs_val.0)
        {
            assert!(lw == rw, "eval_binop: operand width mismatch ({lw} vs {rw})");
            let (w, a, b) = (*lw, *a, *b);
            // A carry/borrow result is one bit wider than its
            // operands, so 64-bit operands would produce a 65-bit
            // result that does not fit `Small`; those fall back to
            // the `BitPat` path below.
            let fits = !matches!(binop.op, BinOp::AddCarry | BinOp::SubBorrow) || w < 64;
            if fits {
                return match binop.op {
                    BinOp::LogicalAnd => self.val_bool((a & 1) != 0 && (b & 1) != 0),
                    BinOp::LogicalOr => self.val_bool((a & 1) != 0 || (b & 1) != 0),
                    BinOp::LogicalXor => self.val_bool(((a & 1) != 0) ^ ((b & 1) != 0)),
                    BinOp::Lt => self.val_bool(a < b),
                    BinOp::Lte => self.val_bool(a <= b),
                    BinOp::Gt => self.val_bool(a > b),
                    BinOp::Gte => self.val_bool(a >= b),
                    BinOp::Eq => self.val_bool(a == b),
                    BinOp::Neq => self.val_bool(a != b),
                    BinOp::Add => Value::from_u64(self.typ_id(), w, a.wrapping_add(b) & mask(w)),
                    BinOp::Sub => Value::from_u64(self.typ_id(), w, a.wrapping_sub(b) & mask(w)),
                    BinOp::AddCarry => {
                        // Both operands are below 2^w, so the sum is
                        // below 2^(w+1) and the widened result needs
                        // no masking.
                        Value::from_u64(self.typ_id(), w + 1, (a as u128 + b as u128) as u64)
                    }
                    BinOp::SubBorrow => {
                        let m = if w + 1 >= 64 { u64::MAX } else { (1u64 << (w + 1)) - 1 };
                        Value::from_u64(
                            self.typ_id(),
                            w + 1,
                            ((a as u128).wrapping_sub(b as u128) as u64) & m,
                        )
                    }
                    BinOp::And => Value::from_u64(self.typ_id(), w, a & b),
                    BinOp::Or => Value::from_u64(self.typ_id(), w, a | b),
                    BinOp::Xor => Value::from_u64(self.typ_id(), w, a ^ b),
                };
            }
        }

        let lhs = lhs_val.bitpat();
        let rhs = rhs_val.bitpat();
        match binop.op {
            BinOp::LogicalAnd => self.val(BitPat::bit(lhs.get_bit(0) && rhs.get_bit(0))),
            BinOp::LogicalOr => self.val(BitPat::bit(lhs.get_bit(0) || rhs.get_bit(0))),
            BinOp::LogicalXor => self.val(BitPat::bit(lhs.get_bit(0) ^ rhs.get_bit(0))),
            BinOp::Lt => self.val(BitPat::bit(lhs.ucmp(&rhs) == std::cmp::Ordering::Less)),
            BinOp::Lte => self.val(BitPat::bit(lhs.ucmp(&rhs) != std::cmp::Ordering::Greater)),
            BinOp::Gt => self.val(BitPat::bit(lhs.ucmp(&rhs) == std::cmp::Ordering::Greater)),
            BinOp::Gte => self.val(BitPat::bit(lhs.ucmp(&rhs) != std::cmp::Ordering::Less)),
            BinOp::Eq => self.val(BitPat::bit(lhs == rhs)),
            BinOp::Neq => self.val(BitPat::bit(lhs != rhs)),
            BinOp::Add => self.val(lhs.add(&rhs)),
            BinOp::Sub => self.val(lhs.sub(&rhs)),
            BinOp::AddCarry | BinOp::SubBorrow => self.val(widened_addsub(binop.op, &lhs, &rhs)),
            BinOp::And => self.val(lhs.and(&rhs)),
            BinOp::Or => self.val(lhs.or(&rhs)),
            BinOp::Xor => self.val(lhs.xor(&rhs)),
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

        // Hot path: the subject fits in a `u64` — no `BitPat` allocation.
        if let InternalValue::Small { width, bits, .. } = &val.0 {
            let (w, a) = (*width, *bits);
            return match un_op.op {
                UnOp::Neg => Value::from_u64(self.typ_id(), w, a.wrapping_neg() & mask(w)),
                UnOp::Inv => Value::from_u64(self.typ_id(), w, !a & mask(w)),
                UnOp::Not => self.val_bool(a == 0),
            };
        }

        let bits = val.bitpat();
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
            .map(|arg| arg.eval(context).bitpat())
            .collect();
        match Self::layouts(context).encode_ctor(self.typ_id(), ctor.symbol_id, &arg_values) {
            Some(bits) => self.val(bits),
            None => self.val_zeros(self.width_of_typ(context)),
        }
    }

    /// Evaluate an enumerant reference: an enum's tag-only ctor value.
    fn eval_enumerant(&self, context: &Context, enumerant: &payload::Enumerant) -> Value {
        match Self::layouts(context).encode_ctor(self.typ_id(), enumerant.symbol_id, &[]) {
            Some(bits) => self.val(bits),
            None => self.val_zeros(self.width_of_typ(context)),
        }
    }

    /// Evaluate a struct literal: concatenate the fields' bits, first
    /// field at the LSB end (struct layouts carry no tag).  This
    /// matches the struct packing used by the Verilog backend and by
    /// `ExprField` projection, where the first declared field occupies
    /// the lowest bits; `concat_msb_first` accumulates above the bits
    /// collected so far, so the fields are consumed in reverse.
    fn eval_struct(&self, context: &Context, struct_: &payload::Struct) -> Value {
        let field_values: Vec<BitPat> = struct_
            .fields
            .iter()
            .map(|(_, field)| field.eval(context).bitpat())
            .collect();
        let mut bits = BitPat::zeros(0);
        for value in field_values.into_iter().rev() {
            bits = bits.concat_msb_first(&value);
        }
        self.val(bits.zext_to(self.width_of_typ(context)))
    }

    /// Evaluate a single-bit index expression: `subject[index]`.
    /// For a memory subject this is a constant-index word read
    /// (`mem[3]`): the result is the element value,
    /// not a bit.
    fn eval_index(&self, context: &Context, index: &payload::Index) -> Value {
        if let Some(value) = self.eval_memory_read(context, &index.subject, index.index as usize) {
            return value;
        }
        let val = index.subject.eval(context);
        self.val_bool(val.get_bit(index.index as usize))
    }

    fn eval_index_dyn(&self, context: &Context, index_dyn: &payload::IndexDyn) -> Value {
        // A dynamic memory read (`mem[dyn i]`): the subject is a
        // reference to a memory component whose index is exactly
        // `Word[k]` (`depth == 2^k`), so it is always in range.
        if let ExprPayload::Reference(reference) = index_dyn.subject.payload() {
            if let Some(words) = context.memory_words(&reference.referent) {
                let index = index_dyn.index.eval(context).to_u64() as usize;
                return words[index].clone();
            }
        }
        let val = index_dyn.subject.eval(context);
        let idx_val = index_dyn.index.eval(context);
        self.val_bool(val.get_bit(idx_val.to_u64() as usize))
    }

    /// A constant-index memory read (`mem[c]`): `Some` of the word
    /// when `subject` references a memory, `None` for the ordinary
    /// bit-select path.
    fn eval_memory_read(
        &self,
        context: &Context,
        subject: &std::sync::Arc<Expr>,
        index: usize,
    ) -> Option<Value> {
        let ExprPayload::Reference(reference) = subject.payload() else {
            return None;
        };
        let words = context.memory_words(&reference.referent)?;
        Some(words.get(index)?.clone())
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
        if hi as usize > val.width() as usize {
            eprintln!("DEBUG index_range: lo={lo} hi={hi} subject_width={} subject_typ={:?} subject_bits_width={}", val.width(), Self::layouts(context).type_of(val.typ_id()), val.width());
        }
        // Small path: slice the inline `u64` directly — same result as
        // `Value::slice` + `Value::new` without materialising a `BitPat`.
        match &val.0 {
            InternalValue::Small { bits, .. } => {
                assert!(lo <= hi, "Value::slice: lo > hi");
                assert!(hi <= val.width(), "Value::slice: hi out of range");
                Value::from_u64(self.typ_id(), hi - lo, (*bits >> lo) & mask(hi - lo))
            }
            _ => self.val(val.slice(lo, hi)),
        }
    }

    /// Evaluate a word-concatenation expression `{a, b, c}`.
    ///
    /// First child is the MSB (leftmost in `{}`), last child is the LSB,
    /// matching Verilog `Concat` semantics and `ExprWord` in `convert_expr`.
    fn eval_word(&self, context: &Context, word: &payload::Word) -> Value {
        let mut bits = BitPat::zeros(0);
        for arg in &word.args {
            let v = arg.eval(context);
            bits = bits.concat_msb_first(&v.bitpat());
        }
        self.val(bits.zext_to(self.width_of_typ(context)))
    }

    /// Zero-extend (or truncate) `val`'s bits to this expression's type
    /// width, keeping the `Small` representation when the result also
    /// fits in 64 bits (`zext`/`trunc`).
    fn val_zext(&self, context: &Context, val: Value) -> Value {
        let w = self.width_of_typ(context);
        match &val.0 {
            InternalValue::Small { bits, .. } if w <= 64 => {
                Value::from_u64(self.typ_id(), w, *bits)
            }
            _ => self.val(val.bitpat().zext_to(w)),
        }
    }

    /// Evaluate a zero-extension: `zext(subject)`.
    ///
    /// Matches Verilog `{repeat(extend_by, 1'b0), inner}` from `convert_zext`.
    /// Upper bits are filled with zeros; the low bits are taken from the subject value.
    fn eval_zext(&self, context: &Context, zext: &payload::Zext) -> Value {
        let val = zext.subject.eval(context);
        self.val_zext(context, val)
    }

    /// Evaluate a sign-extension: `sext(subject)`.
    ///
    /// Matches Verilog `{repeat(extend_by, inner[msb]), inner}` from `convert_sext`.
    /// The MSB of the subject is replicated into all upper bits.
    fn eval_sext(&self, context: &Context, sext: &payload::Sext) -> Value {
        let val = sext.subject.eval(context);
        let w = self.width_of_typ(context);
        if let InternalValue::Small { width, bits, .. } = &val.0 {
            if w <= 64 {
                let out = if w >= *width {
                    let msb = if *width == 0 { false } else { (bits >> (*width - 1)) & 1 != 0 };
                    if msb { bits | (mask(w) & !mask(*width)) } else { bits & mask(w) }
                } else {
                    bits & mask(w)
                };
                return Value::from_u64(self.typ_id(), w, out);
            }
        }
        self.val(val.bitpat().sext_to(w))
    }

    /// Evaluate a truncation: `trunc(subject)`.
    ///
    /// The low `target_width` bits are taken from the subject value.
    /// If subject is wider than target, the upper bits are discarded.
    fn eval_trunc(&self, context: &Context, trunc: &payload::Trunc) -> Value {
        let val = trunc.subject.eval(context);
        self.val_zext(context, val)
    }

    fn eval_any(&self, context: &Context, any: &payload::Any) -> Value {
        let subject_value = any.subject.eval(context);
        self.val_bool(!subject_value.is_zero())
    }

    fn eval_all(&self, context: &Context, all: &payload::All) -> Value {
        let subject_value = all.subject.eval(context);
        self.val_bool(subject_value.is_all_ones())
    }
}


