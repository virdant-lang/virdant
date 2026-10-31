//! The simulated `Value`: a bit pattern tagged with its (semantic)
//! type.  Formerly defined inside the interpreter module (`sim/eval.rs`);
//! the interpreter is gone, but every commit boundary still speaks
//! `Value` (`State`, the VM's `Ret`, the public `Sim` API).

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
    pub(crate) fn bitpat(&self) -> BitPat {
        match &self.0 {
            InternalValue::Big { bits, .. } => bits.clone(),
            InternalValue::Small { width, bits, .. } => BitPat::from_u64(*width, *bits),
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

