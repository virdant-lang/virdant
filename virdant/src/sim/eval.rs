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

use indexmap::IndexMap;

use crate::common::{BinOp, UnOp, Width};
use crate::sim::bits::BitPat;
use crate::sim::layout::{TypeId, TypeLayoutIndex};
use crate::sim::payload;
use crate::types::Type;
use crate::sim::expr::{Expr, ExprPayload, Referent};
use crate::analysis::elaboration::SignalId;

/// A simulated value: a bit pattern tagged with its (semantic) type.
///
/// `Value` is a thin newtype over the private [`InternalValue`] enum, which
/// has two representations:
///
/// - `Small` — the hot path.  For patterns of at most 64 bits the whole
///   pattern is one masked `u64` plus an interned [`TypeId`]:
///   constructing, cloning, and comparing a value never touches the heap.
/// - `Big` — the original representation (a heap-allocated [`BitPat`])
///   for anything wider than 64 bits (large words, aggregates).
///
/// The type tag is the interned [`TypeId`] of the semantic type (see
/// `sim::layout`); resolve it back to a `Type` through the circuit's
/// `TypeLayoutIndex` (`type_of`).  Gate logic decodes the bits on
/// demand; aggregate types (unions, enums, structs) are stored
/// pre-encoded.
#[derive(Clone, Debug)]
pub struct Value(InternalValue);

#[derive(Clone, Debug)]
enum InternalValue {
    /// Original representation: an arbitrary-width [`BitPat`] (limb vector
    /// on the heap).  Used for patterns wider than 64 bits.
    Big { typ: TypeId, bits: BitPat },
    /// Hot path for patterns of at most 64 bits: the whole (masked)
    /// pattern in a single `u64`.
    Small { typ: TypeId, width: Width, bits: u64 },
}

/// Mask with the low `width` bits set (assumes `width <= 64`).
fn mask(width: Width) -> u64 {
    if width >= 64 { u64::MAX } else { (1u64 << width) - 1 }
}

impl PartialEq for Value {
    /// Values are equal when they carry the same type tag and the same
    /// bits — regardless of which representation (`Small`/`Big`) each
    /// side uses.  Interned `TypeId`s make the tag comparison a plain
    /// `u32` compare.
    fn eq(&self, other: &Self) -> bool {
        self.typ_id() == other.typ_id() && self.same_bits(other)
    }
}

impl Eq for Value {}

impl Value {
    pub fn new(typ: TypeId, bits: BitPat) -> Value {
        if bits.width() <= 64 {
            Value(InternalValue::Small {
                typ,
                width: bits.width(),
                bits: bits.to_u64() & mask(bits.width()),
            })
        } else {
            Value(InternalValue::Big { typ, bits })
        }
    }

    /// A `width`-bit value of `typ` holding `bits` (truncated, masked to
    /// `width`).  Only meaningful for `width <= 64`; wider patterns must
    /// go through [`Value::new`].
    pub(crate) fn from_u64(typ: TypeId, width: Width, bits: u64) -> Value {
        if width <= 64 {
            Value(InternalValue::Small { typ, width, bits: bits & mask(width) })
        } else {
            Value(InternalValue::Big { typ, bits: BitPat::from_u64(width, bits) })
        }
    }

    /// A single-bit value of `typ` (for `Bit`/`Clock`/`Reset`-typed
    /// signals).
    pub fn from_bool(typ: TypeId, b: bool) -> Value {
        Value(InternalValue::Small { typ, width: 1, bits: b as u64 })
    }

    /// The same bits re-tagged with `typ` (keeps the representation).
    pub(crate) fn with_typ(&self, typ: TypeId) -> Value {
        match &self.0 {
            InternalValue::Big { bits, .. } => Value(InternalValue::Big { typ, bits: bits.clone() }),
            InternalValue::Small { width, bits, .. } => {
                Value(InternalValue::Small { typ, width: *width, bits: *bits })
            }
        }
    }

    /// The interned type id of this value's type tag.  Resolve to a
    /// `Type` through `TypeLayoutIndex::type_of`.
    pub fn typ_id(&self) -> TypeId {
        match &self.0 {
            InternalValue::Big { typ, .. } | InternalValue::Small { typ, .. } => *typ,
        }
    }

    /// The same value with bit 0 flipped (used for free-running clock
    /// toggles; keeps the type tag).
    pub(crate) fn toggled(&self) -> Value {
        match &self.0 {
            InternalValue::Small { typ, width, bits } => {
                Value(InternalValue::Small { typ: *typ, width: *width, bits: bits ^ 1 })
            }
            InternalValue::Big { typ, bits } => {
                let mut bits = bits.clone();
                bits.set_bit(0, !bits.get_bit(0));
                Value(InternalValue::Big { typ: *typ, bits })
            }
        }
    }

    pub fn width(&self) -> Width {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.width(),
            InternalValue::Small { width, .. } => *width,
        }
    }

    /// The bit at position `i` (0 = LSB).  Returns false for `i >= width`.
    pub fn get_bit(&self, i: usize) -> bool {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.get_bit(i),
            InternalValue::Small { width, bits, .. } => {
                if i >= *width as usize { return false; }
                (bits >> i) & 1 != 0
            }
        }
    }

    /// The low 64 bits (truncated).
    pub fn to_u64(&self) -> u64 {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.to_u64(),
            InternalValue::Small { bits, .. } => *bits,
        }
    }

    /// The whole pattern as a bit string, MSB first.
    pub fn to_bit_string(&self) -> String {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.to_bit_string(),
            InternalValue::Small { width, bits, .. } => {
                let mut s = String::with_capacity(*width as usize);
                for i in (0..*width).rev() {
                    s.push(if (bits >> i) & 1 != 0 { '1' } else { '0' });
                }
                s
            }
        }
    }

    /// True when every bit is zero.
    pub fn is_zero(&self) -> bool {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.is_zero(),
            InternalValue::Small { bits, .. } => *bits == 0,
        }
    }

    /// True when every bit within `width` is one.
    pub fn is_all_ones(&self) -> bool {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.is_all_ones(),
            InternalValue::Small { width, bits, .. } => *width == 64 || *bits == mask(*width),
        }
    }

    /// The bits as a [`BitPat`] (materialises the `Small` hot path; the
    /// returned pattern is a fresh heap allocation in that case).
    pub(crate) fn bitpat(&self) -> BitPat {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.clone(),
            InternalValue::Small { width, bits, .. } => BitPat::from_u64(*width, *bits),
        }
    }

    /// Bits `[lo, hi)` (MSB-exclusive convention matches Virdant range
    /// indexing: `x[hi..lo]` has `hi` exclusive).  Panics if `lo > hi`
    /// or `hi > width`.  The result is a fresh [`BitPat`]; wrap it back
    /// into a `Value` with [`Value::new`].
    pub(crate) fn slice(&self, lo: u16, hi: u16) -> BitPat {
        assert!(lo <= hi, "Value::slice: lo > hi");
        assert!(hi <= self.width(), "Value::slice: hi out of range");
        match &self.0 {
            InternalValue::Small { bits, .. } => BitPat::from_u64(hi - lo, (bits >> lo) & mask(hi - lo)),
            InternalValue::Big { bits, .. } => bits.slice(lo, hi),
        }
    }

    /// True when the value's bits equal `other`'s (which must have the
    /// same width; unequal widths compare unequal).
    pub(crate) fn bits_eq(&self, other: &BitPat) -> bool {
        if self.width() != other.width() {
            return false;
        }
        match &self.0 {
            InternalValue::Big { bits, .. } => bits == other,
            InternalValue::Small { bits, .. } => (other.to_u64() & mask(other.width())) == *bits,
        }
    }

    /// Bit-wise equality against another `Value` (ignores the type tag;
    /// [`PartialEq for Value`] compares the tag too).
    fn same_bits(&self, other: &Value) -> bool {
        if self.width() != other.width() {
            return false;
        }
        match (&self.0, &other.0) {
            (InternalValue::Small { bits: a, .. }, InternalValue::Small { bits: b, .. }) => a == b,
            (InternalValue::Big { bits: a, .. }, InternalValue::Big { bits: b, .. }) => a == b,
            _ => self.bitpat() == other.bitpat(),
        }
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
}

impl Context {
    pub fn new(entries: Vec<(Referent, Value)>, layouts: TypeLayoutIndex) -> Context {
        let mut context: IndexMap<Referent, Value> =
            IndexMap::with_capacity(entries.len());
        for (r, v) in entries {
            context.insert(r, v);
        }
        Context { layouts, context }
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
        Context { layouts: self.layouts.clone(), context }
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
        state: &crate::sim::state::State,
        signal_id: SignalId,
    ) -> Context {
        let entries = circuit
            .referents(signal_id)
            .iter()
            .map(|(r, id)| (r.clone(), state.get(*id)))
            .collect();
        Context::new(entries, circuit.type_layout_index.clone())
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
            ExprPayload::When(when) => self.eval_when(context, when),
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
                    let Some((slot, payloads)) =
                        layouts.decode_ctor(subject.typ_id(), &subject.bitpat())
                    else {
                        continue;
                    };
                    if slot.symbol_id != *symbol_id {
                        continue;
                    }
                    let extra: Vec<(Referent, Value)> = bound_vars
                        .iter()
                        .zip(payloads.iter())
                        .zip(slot.param_types.iter())
                        .map(|(((_var_name, var_loc), bits), typ)| {
                            (
                                Referent::Location(var_loc.clone()),
                                Value::new(layouts.type_id(typ), bits.clone()),
                            )
                        })
                        .collect();
                    let arm_context = context.extend(extra);
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
            }
        }
        // No arm matched and no else arm — zero bits of the result type.
        self.val_zeros(self.width_of_typ(context))
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

        // Hot path: both operands fit in a `u64` — compute in machine
        // words, no `BitPat` allocation.
        if let (
            InternalValue::Small { width: lw, bits: a, .. },
            InternalValue::Small { width: rw, bits: b, .. },
        ) = (&lhs_val.0, &rhs_val.0)
        {
            assert!(lw == rw, "eval_binop: operand width mismatch ({lw} vs {rw})");
            let (w, a, b) = (*lw, *a, *b);
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
                BinOp::And => Value::from_u64(self.typ_id(), w, a & b),
                BinOp::Or => Value::from_u64(self.typ_id(), w, a | b),
                BinOp::Xor => Value::from_u64(self.typ_id(), w, a ^ b),
            };
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
    /// field at the MSB end (struct layouts carry no tag).
    fn eval_struct(&self, context: &Context, struct_: &payload::Struct) -> Value {
        let field_values: Vec<BitPat> = struct_
            .fields
            .iter()
            .map(|(_, field)| field.eval(context).bitpat())
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
        self.val_bool(val.get_bit(index.index as usize))
    }

    fn eval_index_dyn(&self, context: &Context, index_dyn: &payload::IndexDyn) -> Value {
        let val = index_dyn.subject.eval(context);
        let idx_val = index_dyn.index.eval(context);
        self.val_bool(val.get_bit(idx_val.to_u64() as usize))
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


