//! The simulated `Value`: a bit pattern tagged with its (semantic)
//! type.  Formerly defined inside the interpreter module (`sim/eval.rs`);
//! now shared by the legacy interpreter (`sim::legacy::eval`), the
//! engines' state, and the public `Sim` API.

use crate::common::Width;
use crate::sim::bits::BitPat;
use crate::sim::layout::TypeId;

/// A simulated value: a bit pattern tagged with its (semantic) type.
///
/// `Value` is a thin newtype over the private [`InternalValue`] enum, which
/// has two representations:
///
/// - `Small` — the hot path.  For patterns of at most 64 bits the whole
///   pattern is one masked `u64` plus an interned [`TypeId`]:
///   constructing, cloning, and comparing a value never touches the heap.
/// - `Big`, the wide representation: a [`BitPat`], which is inline
///   (allocation-free) up to 128 bits and heap-backed beyond, for
///   anything wider than 64 bits (large words, aggregates).
///
/// The type tag is the interned [`TypeId`] of the semantic type (see
/// `sim::layout`); resolve it back to a `Type` through the circuit's
/// `TypeLayoutIndex` (`type_of`).  Gate logic decodes the bits on
/// demand; aggregate types (unions, enums, structs) are stored
/// pre-encoded.
#[derive(Clone, Debug)]
pub struct Value(pub(crate) InternalValue);

/// The two representations behind [`Value`].  `pub(crate)` (variant
/// fields inherit the enum's visibility) so the legacy interpreter's
/// hot paths (`eval_binop`, `eval_unop`, `eval_index_range`) can match
/// on the inline `Small` shape directly, without materialising a
/// [`BitPat`].
#[derive(Clone, Debug)]
pub(crate) enum InternalValue {
    /// Wide representation: an arbitrary-width [`BitPat`] (inline up to
    /// 128 bits, limb vector on the heap beyond).  Used for patterns
    /// wider than 64 bits.
    Big { typ: TypeId, bits: BitPat },
    /// Hot path for patterns of at most 64 bits: the whole (masked)
    /// pattern in a single `u64`.
    Small { typ: TypeId, width: Width, bits: u64 },
}

/// Mask with the low `width` bits set (assumes `width <= 64`).
pub(crate) fn mask(width: Width) -> u64 {
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
    /// The narrow-value store's addressing constants for the JIT's
    /// direct state reads (`sim::vm::jit`): the byte stride between
    /// consecutive components' live values in `State::vals` (exactly
    /// `size_of::<Value>()` — `Vec` element strides), and the offset of
    /// the `Small` variant's `bits` field within that stride.  A
    /// component whose type width is at most 64 always stores the
    /// `Small` variant — every constructor narrows that way
    /// (`Value::new`, `from_u64`, `from_bool`, and the init strategy
    /// all route through the same width test) — so such a component's
    /// live bits sit at a fixed address for the whole engine's
    /// lifetime, which is what the compiled phase code addresses.
    /// Wider components store `Big` and are heap-backed: the JIT's
    /// phase eligibility refuses any `Load` of one.
    ///
    /// The offset is found by probe, not assumed: a `Small` value with
    /// a distinctive 8-byte pattern is scanned for the pattern's byte
    /// offset, so any `repr(Rust)` layout the compiler picks (field
    /// reordering, niche-encoded discriminants) is measured, never
    /// guessed.  `typ` only tags the probe value; the layout does not
    /// depend on which type it names.
    #[cfg(feature = "jit")]
    pub(crate) fn jit_layout(typ: TypeId) -> (usize, usize) {
        const MAGIC: u64 = 0x5A5A_A5A5_3C3C_C3C3;
        let v = Value::from_u64(typ, 64, MAGIC);
        let size = std::mem::size_of::<Value>();
        let base = &v as *const Value as *const u8;
        let mut offset = None;
        for i in 0..=(size - 8) {
            let mut word = 0u64;
            for b in 0..8 {
                // Aligned byte-wise read of the value's own bytes: no
                // unaligned load, no write, valid for the probe's scope.
                word |= (unsafe { *base.add(i + b) } as u64) << (8 * b);
            }
            if word == MAGIC {
                offset = Some(i);
                break;
            }
        }
        let offset = offset.expect("Value layout probe: Small.bits pattern not found");
        (size, offset)
    }

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

    /// The raw bits and width when the value is stored in the narrow
    /// (<= 64-bit) representation: `None` for wide values.  The
    /// bits-only commit path (PERF-IDEAS item 10) compares raw
    /// integers against these instead of building and comparing
    /// `Value`s.
    pub(crate) fn as_narrow(&self) -> Option<(Width, u64)> {
        match &self.0 {
            InternalValue::Small { width, bits, .. } => Some((*width, *bits)),
            InternalValue::Big { .. } => None,
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
    pub fn bitpat(&self) -> BitPat {
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


#[cfg(test)]
mod tests {
    use super::*;
    use crate::sim::layout::TypeInterner;
    use crate::types::Type;

    /// The JIT's direct-state-read invariant, exercised end to end:
    /// two `Small` values in a `Vec` — the engine's exact storage
    /// shape (`State::vals`) — read back through the probed stride and
    /// offset give exactly each value's bits.  This is the addressing
    /// contract `vm::jit`'s compiled code relies on; the probe must
    /// track whatever layout the compiler picked, not assume one.
    #[cfg(feature = "jit")]
    #[test]
    fn jit_layout_probe_addresses_small_bits_correctly() {
        let mut interner = TypeInterner::new();
        let typ = interner.intern(&Type::Word(64));
        let magic0: u64 = 0x1234_5678_9abc_def0;
        let magic1: u64 = 0x0f1e_2d3c_4b5a_6978;
        let vals = vec![
            Value::from_u64(typ, 64, magic0),
            Value::from_u64(typ, 64, magic1),
        ];
        let (stride, off) = Value::jit_layout(typ);
        assert_eq!(stride, std::mem::size_of::<Value>());
        let base = vals.as_ptr() as *const u8;
        let read = |i: usize| unsafe { *(base.add(i * stride + off) as *const u64) };
        assert_eq!(read(0), magic0);
        assert_eq!(read(1), magic1);
    }
}
