//! Arbitrary-width two-state bit patterns.
//!
//! Every simulated value is a `BitPat`: a `width`-bit vector of 0/1
//! bits, stored little-endian in 64-bit limbs.  Gate logic decodes the
//! bits on demand (see `eval`); this module only knows about bits.
//!
//! Bit *n* is limb `n / 64`, bit `n % 64`.  Bits at or above `width`
//! are always zero (kept normalised by every constructor and mutator).
//!
//! Two representations behind one enum: patterns of at most 128 bits
//! (two limbs) live inline in a fixed `[u64; 2]`, so construction,
//! cloning, arithmetic, and comparison of ordinary word-width values
//! never touch the heap; wider patterns keep the original limb vector.

use crate::common::Width;

/// A `width`-bit pattern of 0/1 bits.
///
/// Public shape unchanged (private fields, the same constructor and
/// method surface as before); the representation behind it:
///
/// - width at most 128: the used limbs in a fixed array (the rest, if
///   any, always zero), so no heap allocation anywhere on this path;
///   the zero limbs keep the derived `PartialEq`/`Hash` comparing
///   normalised data;
/// - width beyond 128: the original heap-backed limb vector.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BitPat {
    width: Width,
    repr: Repr,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Repr {
    /// The used limbs in a fixed array; limbs beyond `words_for(width)`
    /// stay zero.
    Inline([u64; 2]),
    /// The original heap-backed limb vector.
    Heap(Vec<u64>),
}

/// Limbs needed to hold `width` bits.
fn words_for(width: Width) -> usize {
    ((width as usize) + 63) / 64
}

/// Limbs an inline `BitPat` can carry (widths up to 128).
const INLINE_WORDS: usize = 2;

/// Mask off bits at or above `width` in the top limb.  `words` must
/// hold exactly `words_for(width)` limbs.
fn mask_top(width: Width, words: &mut [u64]) {
    let excess = (width as usize) % 64;
    if excess != 0 {
        let last = words.len() - 1;
        words[last] &= (1u64 << excess) - 1;
    }
}

impl BitPat {
    /// Construct a `width`-bit pattern from `fill`, which receives a
    /// zero-initialised limb slice of exactly the used limbs
    /// (`words_for(width)` of them: inline up to 128 bits, heap
    /// beyond).  Nothing allocates on the inline path.
    fn build(width: Width, fill: impl FnOnce(&mut [u64])) -> BitPat {
        let n = words_for(width);
        if n <= INLINE_WORDS {
            let mut words = [0u64; INLINE_WORDS];
            fill(&mut words[..n]);
            BitPat { width, repr: Repr::Inline(words) }
        } else {
            let mut words = vec![0u64; n];
            fill(&mut words);
            BitPat { width, repr: Repr::Heap(words) }
        }
    }

    /// The used limbs (those carrying bits of `width`).
    fn words(&self) -> &[u64] {
        match &self.repr {
            Repr::Inline(words) => &words[..words_for(self.width)],
            Repr::Heap(words) => words,
        }
    }

    /// Mutable used limbs (those carrying bits of `width`).  Limbs
    /// beyond these are never written, so they stay zero.
    fn words_mut(&mut self) -> &mut [u64] {
        let width = self.width;
        match &mut self.repr {
            Repr::Inline(words) => &mut words[..words_for(width)],
            Repr::Heap(words) => words,
        }
    }

    /// A pattern of `width` zero bits.
    pub fn zeros(width: Width) -> BitPat {
        BitPat::build(width, |_| {})
    }

    /// A pattern of `width` one-bits.
    pub fn ones(width: Width) -> BitPat {
        BitPat::build(width, |words| {
            words.iter_mut().for_each(|w| *w = u64::MAX);
            mask_top(width, words);
        })
    }

    /// A single-bit pattern.
    pub fn bit(b: bool) -> BitPat {
        BitPat { width: 1, repr: Repr::Inline([b as u64, 0]) }
    }

    /// A `width`-bit pattern holding `value` (truncated).
    pub fn from_u64(width: Width, value: u64) -> BitPat {
        BitPat::build(width, |words| {
            if let Some(first) = words.first_mut() {
                *first = value;
            }
            mask_top(width, words);
        })
    }

    /// A `width`-bit pattern from `bits`, MSB first: `bits[0]` is the
    /// most significant bit.  Panics if `bits.len() != width`.
    pub fn from_bits_msb_first(width: Width, bits: &[bool]) -> BitPat {
        assert!(bits.len() == width as usize, "BitPat::from_bits_msb_first: length mismatch");
        let mut out = BitPat::zeros(width);
        for (i, b) in bits.iter().enumerate() {
            if *b {
                out.set_bit((width - 1 - i as u16) as usize, true);
            }
        }
        out
    }

    /// The bit at position `i` (0 = LSB).  Returns false for `i >= width`.
    pub fn get_bit(&self, i: usize) -> bool {
        if i >= self.width() as usize { return false; }
        (self.words()[i / 64] >> (i % 64)) & 1 != 0
    }

    pub fn set_bit(&mut self, i: usize, b: bool) {
        assert!(i < self.width() as usize, "BitPat::set_bit out of range");
        if b { self.words_mut()[i / 64] |= 1 << (i % 64); }
        else { self.words_mut()[i / 64] &= !(1 << (i % 64)); }
    }

    pub fn width(&self) -> Width {
        self.width
    }

    /// True when every bit is zero.
    pub fn is_zero(&self) -> bool {
        self.words().iter().all(|w| *w == 0)
    }

    /// True when every bit within `width` is one.
    pub fn is_all_ones(&self) -> bool {
        *self == BitPat::ones(self.width())
    }

    /// The low 64 bits (truncated).  Gate logic on designs whose values
    /// fit in a machine word goes through this.
    pub fn to_u64(&self) -> u64 {
        self.words().first().copied().unwrap_or(0)
    }

    /// The whole pattern as a bit string, MSB first.
    pub fn to_bit_string(&self) -> String {
        let mut s = String::with_capacity(self.width() as usize);
        for i in (0..self.width()).rev() {
            s.push(if self.get_bit(i as usize) { '1' } else { '0' });
        }
        s
    }

    pub fn set(&mut self, other: &BitPat) {
        assert!(self.width() == other.width(), "BitPat::set width mismatch");
        let src = other.words();
        self.words_mut().copy_from_slice(src);
    }

    /// Bits `[lo, hi)` (MSB-exclusive convention matches Virdant range
    /// indexing: `x[hi..lo]` has `hi` exclusive).  Panics if `lo > hi`
    /// or `hi > width`.
    pub fn slice(&self, lo: u16, hi: u16) -> BitPat {
        assert!(lo <= hi, "BitPat::slice: lo > hi");
        assert!(hi <= self.width(), "BitPat::slice: hi out of range");
        let width = hi - lo;
        let mut out = BitPat::zeros(width);
        for i in 0..width as usize {
            out.set_bit(i, self.get_bit(lo as usize + i));
        }
        out
    }

    /// Concatenation with `self` in the MSB position: the result is
    /// `width + other.width` bits, `self`'s bits above `other`'s.
    pub fn concat_msb_first(&self, other: &BitPat) -> BitPat {
        let width = self.width() + other.width();
        let mut out = BitPat::zeros(width);
        for i in 0..other.width() as usize {
            out.set_bit(i, other.get_bit(i));
        }
        for i in 0..self.width() as usize {
            out.set_bit(other.width() as usize + i, self.get_bit(i));
        }
        out
    }

    /// Zero-extend (or truncate) to `width`.
    pub fn zext_to(&self, width: Width) -> BitPat {
        if width >= self.width() {
            let src = self.words();
            let copy = src.len().min(words_for(width));
            BitPat::build(width, |out| {
                out[..copy].copy_from_slice(&src[..copy]);
                mask_top(width, out);
            })
        } else {
            self.slice(0, width)
        }
    }

    /// Sign-extend to `width` by replicating the MSB.
    pub fn sext_to(&self, width: Width) -> BitPat {
        if width <= self.width() {
            return self.slice(0, width);
        }
        let msb = self.get_bit(self.width() as usize - 1);
        let mut out = self.zext_to(width);
        if msb {
            for i in self.width() as usize..width as usize {
                out.set_bit(i, true);
            }
        }
        out
    }

    pub fn shl(&self, amount: u32) -> BitPat {
        if amount as usize >= self.width() as usize {
            return BitPat::zeros(self.width());
        }
        let mut out = BitPat::zeros(self.width());
        for i in amount as usize..self.width() as usize {
            out.set_bit(i, self.get_bit(i - amount as usize));
        }
        out
    }

    pub fn shr(&self, amount: u32) -> BitPat {
        if amount as usize >= self.width() as usize {
            return BitPat::zeros(self.width());
        }
        let mut out = BitPat::zeros(self.width());
        for i in 0..self.width() as usize - amount as usize {
            out.set_bit(i, self.get_bit(i + amount as usize));
        }
        out
    }

    pub fn sar(&self, amount: u32) -> BitPat {
        let msb = self.get_bit(self.width() as usize - 1);
        let shifted = self.shr(amount);
        if msb {
            let mut out = shifted;
            let width = self.width() as usize;
            let amount = amount as usize;
            if amount < width {
                for i in (width - amount)..width {
                    out.set_bit(i, true);
                }
            } else {
                for i in 0..width {
                    out.set_bit(i, true);
                }
            }
            out
        } else {
            shifted
        }
    }

    pub fn not(&self) -> BitPat {
        BitPat::ones(self.width()).xor(self)
    }

    pub fn and(&self, other: &BitPat) -> BitPat {
        assert!(self.width() == other.width(), "BitPat::and width mismatch");
        let a = self.words();
        let b = other.words();
        BitPat::build(self.width(), |out| {
            for ((w, x), y) in out.iter_mut().zip(a).zip(b) {
                *w = x & y;
            }
        })
    }

    pub fn or(&self, other: &BitPat) -> BitPat {
        assert!(self.width() == other.width(), "BitPat::or width mismatch");
        let a = self.words();
        let b = other.words();
        BitPat::build(self.width(), |out| {
            for ((w, x), y) in out.iter_mut().zip(a).zip(b) {
                *w = x | y;
            }
        })
    }

    pub fn xor(&self, other: &BitPat) -> BitPat {
        assert!(self.width() == other.width(), "BitPat::xor width mismatch");
        let a = self.words();
        let b = other.words();
        BitPat::build(self.width(), |out| {
            for ((w, x), y) in out.iter_mut().zip(a).zip(b) {
                *w = x ^ y;
            }
        })
    }

    /// Wrapping add (mod 2^width).
    pub fn add(&self, other: &BitPat) -> BitPat {
        assert!(self.width() == other.width(), "BitPat::add width mismatch");
        let a = self.words();
        let b = other.words();
        let mut carry = 0u64;
        BitPat::build(self.width(), |out| {
            for i in 0..out.len() {
                let (s1, c1) = a[i].overflowing_add(b[i]);
                let (s2, c2) = s1.overflowing_add(carry);
                out[i] = s2;
                carry = (c1 as u64) + (c2 as u64);
            }
            mask_top(self.width(), out);
        })
    }

    /// Wrapping sub (mod 2^width).
    pub fn sub(&self, other: &BitPat) -> BitPat {
        assert!(self.width() == other.width(), "BitPat::sub width mismatch");
        let a = self.words();
        let b = other.words();
        let mut borrow = 0u64;
        BitPat::build(self.width(), |out| {
            for i in 0..out.len() {
                let (d1, b1) = a[i].overflowing_sub(b[i]);
                let (d2, b2) = d1.overflowing_sub(borrow);
                out[i] = d2;
                borrow = (b1 as u64) + (b2 as u64);
            }
            mask_top(self.width(), out);
        })
    }

    /// Wrapping negation (mod 2^width).
    pub fn neg(&self) -> BitPat {
        BitPat::zeros(self.width()).sub(self)
    }

    /// Unsigned comparison.  Returns `Ordering` of the two patterns.
    pub fn ucmp(&self, other: &BitPat) -> std::cmp::Ordering {
        assert!(self.width() == other.width(), "BitPat::ucmp width mismatch");
        let a = self.words();
        let b = other.words();
        for i in (0..a.len()).rev() {
            if a[i] != b[i] {
                return a[i].cmp(&b[i]);
            }
        }
        std::cmp::Ordering::Equal
    }

    /// Pseudorandom pattern of `width` bits, from `state` (updated in
    /// place).  A xorshift64* generator — `rand`-free, adequate for
    /// uninitialized-value randomization.
    pub fn random(width: Width, state: &mut u64) -> BitPat {
        BitPat::build(width, |out| {
            for w in out.iter_mut() {
                *state ^= *state >> 12;
                *state ^= *state << 25;
                *state ^= *state >> 27;
                *w = state.wrapping_mul(0x2545F4914F6CDD1D);
            }
            mask_top(width, out);
        })
    }
}