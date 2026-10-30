//! The VM instruction set for the compiled simulator.
//!
//! This is a pure data module: `Ins`, `Dst`, `Src`, `Truthiness`,
//! `CodeRange`, `FrameLayout`, `Program`, and `FallbackReason`. The
//! executor (`vm::exec`) interprets `Ins` against a `Frame`; the
//! compiler (`vm::compile`) produces `Program`s.
//!
//! Conventions that the executor relies on:
//!
//! * *Slot-width invariant*: whenever the executor stores into a slot
//!   it masks the stored bits to that slot's width (each instruction
//!   carries the immediates it needs for its own masking). Reads may
//!   therefore assume the bits above a slot's width are zero; no
//!   extra masking is done at read time.
//! * `Value`s are constructed only at `Ret`/`RetWide` (and in the
//!   interpreter fallback). The frame holds raw `u64` narrow slots
//!   and `BitPat` wide slots, so the 64-bit-and-narrower path
//!   performs no allocation.
//!
//! The whole ISA is emitted by the compiler, including the
//! control-flow forms (`Jmp`, `BrTruthy`, `TestTag`, `TestTagWide`,
//! `TestBits`, `TestBit`, `Extract`) that the `when`/`match`
//! lowering transcribes `eval_when`/`eval_match` with.

use crate::analysis::elaboration::SignalId;
use crate::analysis::symbols::SymbolId;
use crate::common::{BinOp, UnOp, Width};
use crate::sim::bits::BitPat;
use crate::sim::layout::{TypeId, TypeLayoutIndex};

pub(super) type Ip = u32;
pub(super) type SlotN = u16;
pub(super) type SlotW = u16;

/// One instruction. Its encoding is ad-hoc and evolves with the
/// optimizer, so it is a plain enum rather than a fixed-width word.
#[derive(Clone, Debug)]
pub(super) enum Ins {
    /// Load a component's live value: narrow (`Self`/`Wire`/`Reg`)
    /// or wide (`Reg` with payload > 64b). Loads mask nothing: the
    /// signal's stored bits are already masked to its type width,
    /// which the compiler has checked equals the destination slot's
    /// width.
    Load {
        signal: SignalId,
        dst: Dst,
    },
    /// A referent that `compute_resolved_referents` could not resolve:
    /// today it evaluates as `Context::get`'s fallback, a width-1
    /// zero value, re-tagged with the reading node's type.  Preserved
    /// here as a constant, with the per-eval `eprintln!` moved to
    /// compile time (once, not per eval).
    LoadUnresolved {
        dst: SlotN,
    },
    /// A constant (narrow destination, `width <= 64`). `bits` is
    /// pre-masked to the destination slot's width by the compiler.
    Const {
        bits: u64,
        dst: SlotN,
    },
    /// A constant with a wide destination (`width > 64`).
    WideConst {
        bits: BitPat,
        dst: SlotW,
    },
    /// An all-zero constant. Narrow and wide destinations alike.
    Zero {
        dst: Dst,
        width: Width,
    },
    /// Unconditional jump (arm convergence and fall-through to the
    /// else/hold path in the `when`/`match` lowering).
    Jmp {
        to: Ip,
    },
    /// Branch if the 1-bit `cond` slot is truthy (`kind`: `Bit` —
    /// nonzero bit; `NonZero`, any nonzero bit pattern).  The
    /// `when`/`match` lowering emits one per condition / match test.
    BrTruthy {
        cond: SlotN,
        kind: Truthiness,
        to: Ip,
    },
    /// Match-arm tag test against a narrow subject: the bits at
    /// `tag_lo` (masked by `tag_mask`) must equal `tag`.  Transcribes
    /// `decode_ctor`'s tag read (`bits.slice(0, tag_width) == tag`).
    TestTag {
        subj: SlotN,
        tag_lo: u16,
        tag_mask: u64,
        tag: u64,
        next_if_match: Ip,
    },
    /// Match-arm tag test against a wide subject: the tag pattern's
    /// bits must equal the subject's low bits.
    TestTagWide {
        subj: SlotW,
        tag: BitPat,
        next_if_match: Ip,
    },
    /// Extract a sub-field of a narrow subject: bits `lo..lo+width`
    /// (the `when`/`match` lowering uses it to bind pattern payloads
    /// and to test padding bits).
    Extract {
        subj: SlotN,
        lo: u16,
        width: u16,
        dst: SlotN,
    },
    /// Extract a sub-field of a wide subject. The result may be
    /// narrow or wide depending on the extracted type's width.
    ExtractWide {
        subj: SlotW,
        lo: u16,
        width: u16,
        dst: Dst,
    },
    /// Pattern test: the subject's bits must equal `value` (already
    /// masked to the subject's width).  Emitted for `WordLit` match
    /// arms on narrow subjects and for the padding-validity check.
    TestBits {
        subj: SlotN,
        value: u64,
        next_if_match: Ip,
    },
    /// Pattern test on a single bit: `1`/`0` only.  Emitted for
    /// `BitLit` match arms.
    TestBit {
        subj: SlotN,
        bit: bool,
        next_if_match: Ip,
    },
    /// A narrow binary op. `lhs`/`rhs` slots are already masked to
    /// `width`; the result is masked to `width` by the executor.
    Bin {
        op: BinOp,
        width: Width,
        lhs: SlotN,
        rhs: SlotN,
        dst: SlotN,
    },
    /// A wide binary op. The result may be narrow (comparisons,
    /// `Eq`/`Neq`) or wide.
    BinWide {
        op: BinOp,
        lhs: SlotW,
        rhs: SlotW,
        dst: Dst,
    },
    /// A narrow unary op.
    Un {
        op: UnOp,
        width: Width,
        subj: SlotN,
        dst: SlotN,
    },
    /// A wide unary op (`Not` yields a narrow result).
    UnWide {
        op: UnOp,
        subj: SlotW,
        dst: Dst,
    },
    /// Encode a union constructor into a narrow result
    /// (`total_width <= 64`): `tag` at `tag_lo`, each argument slot
    /// spliced at its declared bit offset.
    EncodeCtor {
        total_width: Width,
        tag: u64,
        tag_lo: u16,
        parts: Box<[(SlotN, u16)]>,
        dst: SlotN,
    },
    /// Encode a union constructor into a wide result
    /// (`total_width > 64`). `typ`/`symbol` go through
    /// `TypeLayoutIndex::encode_ctor`; `args` are (source, bit
    /// offset) pairs.
    EncodeCtorWide {
        typ: TypeId,
        symbol: SymbolId,
        total_width: Width,
        args: Box<[(Src, Width)]>,
        dst: SlotW,
    },
    /// MSB-first concatenation with static offsets: one instruction
    /// covers `eval_word` (`{a, b, c}`) and `eval_struct` (field
    /// concatenation).  Each part is `(source, bit offset, width)`;
    /// parts may be narrow or wide (a wide part into a narrow
    /// destination is rejected at compile time).
    Splice {
        total_width: Width,
        parts: Box<[(Src, u16, u16)]>,
        dst: Dst,
    },
    /// Static bit index into a narrow subject (word-representable
    /// result). Indices at or past 64 yield 0 (the slot's bits are
    /// zero above its width, and `BitPat::get_bit` is false there).
    Index {
        subj: SlotN,
        index: u16,
        dst: SlotN,
    },
    /// Static bit index into a wide subject.
    IndexWide {
        subj: SlotW,
        index: u16,
        dst: SlotN,
    },
    /// Dynamic bit index into a narrow subject: `bits >> index & 1`,
    /// 0 for `index >= 64`.
    IndexDyn {
        subj: SlotN,
        index: SlotN,
        dst: SlotN,
    },
    /// Dynamic bit index into a wide subject.
    IndexDynWide {
        subj: SlotW,
        index: SlotN,
        dst: SlotN,
    },
    /// Static bit range out of a narrow subject:
    /// `(bits >> lo) & mask(hi - lo)`, 0 for `lo >= 64`.
    IndexRange {
        subj: SlotN,
        lo: u16,
        hi: u16,
        dst: SlotN,
    },
    /// Static bit range out of a wide subject. The result may be
    /// narrow or wide depending on the slice type's width.
    IndexRangeWide {
        subj: SlotW,
        lo: u16,
        hi: u16,
        dst: Dst,
    },
    /// Zero-extend a narrow subject to `out_width`.
    Zext {
        subj: SlotN,
        out_width: Width,
        dst: Dst,
    },
    /// Zero-extend a wide subject to `out_width`.
    ZextWide {
        subj: SlotW,
        out_width: Width,
        dst: Dst,
    },
    /// Sign-extend a narrow subject of `subj_width` bits to
    /// `out_width`.
    Sext {
        subj: SlotN,
        subj_width: Width,
        out_width: Width,
        dst: Dst,
    },
    /// Sign-extend a wide subject to `out_width`.
    SextWide {
        subj: SlotW,
        out_width: Width,
        dst: Dst,
    },
    /// A no-op move between narrow slots (the `when`/`match` lowering
    /// converges each arm's value onto the shared result slot with
    /// it; slot fusion in the optimizer removes most of them).
    Move {
        src: SlotN,
        dst: SlotN,
    },
    /// A no-op move between wide slots (same role as `Move`).
    MoveWide {
        src: SlotW,
        dst: SlotW,
    },
    /// Reduction `Any` over a narrow subject.
    Any {
        subj: SlotN,
        dst: SlotN,
    },
    /// Reduction `All` over a narrow subject: `bits == mask(width)`.
    All {
        subj: SlotN,
        width: Width,
        dst: SlotN,
    },
    /// Reduction `Any` over a wide subject.
    AnyWide {
        subj: SlotW,
        dst: SlotN,
    },
    /// Reduction `All` over a wide subject.
    AllWide {
        subj: SlotW,
        dst: SlotN,
    },
    /// A construct this compiler cannot lower (`Method`, `Fn`,
    /// `StrLit`): execution aborts with `unimplemented!`, the same
    /// lazy failure as today's engine.
    Trap,
    /// Return a narrow driver value. The driver's `TypeId`/width are
    /// attached by the executor from the `Program` (not encoded per
    /// instruction).
    Ret {
        src: SlotN,
    },
    /// Return a wide driver value.
    RetWide {
        src: SlotW,
    },
}

/// Where an instruction writes its result. `Narrow` slots are raw
/// `u64` frame entries; `Wide` slots are `BitPat` frame entries.
#[derive(Clone, Copy, Debug)]
pub(super) enum Dst {
    Narrow(SlotN),
    Wide(SlotW),
}

/// Where an instruction reads a value from.
#[derive(Clone, Copy, Debug)]
pub(super) enum Src {
    Narrow(SlotN),
    Wide(SlotW),
}

/// How a condition slot is tested by `BrTruthy`.
#[derive(Clone, Copy, Debug)]
pub(super) enum Truthiness {
    /// A 1-bit value: truthy iff the bit is nonzero.
    Bit,
    /// Any nonzero bit pattern is truthy.
    NonZero,
}

/// A compiled driver body: a contiguous `Ins` range ending in a
/// `Ret`/`RetWide` (or running into the next range's `Ret`).
#[derive(Clone, Copy, Debug)]
pub(super) struct CodeRange {
    pub(super) start: Ip,
    pub(super) len: u32,
}

/// How many narrow/wide slots a driver's frame needs, plus the static
/// width of each wide slot (needed to build zero-extended patterns
/// without re-deriving widths at execution time).
#[derive(Clone, Debug, Default)]
pub(super) struct FrameLayout {
    pub(super) n_narrow: u16,
    pub(super) n_wide: u16,
    /// Static widths of the wide slots, in slot order.
    pub(super) wide_widths: Box<[Width]>,
}

/// Why a driver could not be compiled to bytecode and must run on
/// the interpreter. The fallback is temporary: later units remove it
/// driver by driver until `entry` is total.
///
/// After the `when`/`match` lowering, only `Unsupported` is emitted;
/// `When` and `Match` remain so the fallback log's reason indices
/// stay stable while the fallback exists.
#[allow(dead_code)] // the retired When/Match reasons
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::sim) enum FallbackReason {
    /// The driver contains a `when` subtree.  No longer emitted by the
    /// `when` lowering; kept so the fallback log's reason indices stay
    /// stable while the fallback exists.
    When = 0,
    /// The driver contains a `match` subtree.  No longer emitted by
    /// the `match` lowering; kept so the fallback log's reason indices
    /// stay stable while the fallback exists.
    Match = 1,
    /// The driver contains a construct this unit's lowering cannot
    /// represent (defensive; should not occur in well-typed designs).
    Unsupported = 2,
}

pub(in crate::sim) const FALLBACK_REASONS: usize = 3;

/// A compiled program: one code range per driver that lowered to
/// bytecode, plus per-driver frames. `entry` is `None` for drivers
/// that fell back to the interpreter.
pub(in crate::sim) struct Program {
    pub(super) code: Box<[Ins]>,
    /// `entry[component]`, the driver's code range, if it compiled.
    pub(super) entry: Vec<Option<CodeRange>>,
    /// `frames[component]`, the frame layout for the driver, valid
    /// iff `entry` is `Some`.
    pub(super) frames: Vec<FrameLayout>,
    /// `driver_typs[component]`, the `TypeId` the driver's `Ret`
    /// attaches to its value (`None` for fallback drivers).
    pub(super) driver_typs: Vec<Option<TypeId>>,
    /// `driver_widths[component]`, the driver value's width (used
    /// for debug validation only).
    pub(super) driver_widths: Vec<Width>,
    /// Type layouts, shared with the `Circuit`.
    pub(super) layouts: TypeLayoutIndex,
    /// Per-reason counts of drivers that fell back (indices are
    /// `FallbackReason` discriminants).  Read by the debug-only
    /// fallback log at `Sim::new`.
    #[cfg_attr(not(debug_assertions), allow(dead_code))]
    pub(super) fallback_counts: [usize; FALLBACK_REASONS],
}

impl Program {
    pub(in crate::sim) fn has_driver(&self, comp: SignalId) -> bool {
        self.entry[comp.index()].is_some()
    }

    /// Debug-build counts of drivers that fell back, by reason (the
    /// fallback log at `Sim::new` reads it).  The counts themselves
    /// are always computed; the accessor is debug-only because only
    /// the debug log consumes it.
    #[cfg(debug_assertions)]
    pub(in crate::sim) fn fallback_counts(&self) -> &[usize; FALLBACK_REASONS] {
        &self.fallback_counts
    }

    /// Debug validation: every range is in bounds and ends in a
    /// `Ret`/`RetWide`/`Trap`, and every slot reference fits its
    /// frame. `debug_assert!`s only.
    pub(super) fn validate(&self) {
        for comp_idx in 0..self.entry.len() {
            let Some(range) = self.entry[comp_idx] else {
                continue;
            };
            let frame = &self.frames[comp_idx];
            let end = range.start + range.len;
            assert!(end as usize <= self.code.len());
            for ip in range.start..end {
                let ins = &self.code[ip as usize];
                ins.validate_slots(&frame);
                ins.validate_targets(range);
            }
            match &self.code[end as usize - 1] {
                Ins::Ret { .. } | Ins::RetWide { .. } | Ins::Trap => {}
                other => panic!("Program::validate: range for component {comp_idx} does not end in Ret/RetWide/Trap: {other:?}"),
            }
        }
    }
}

impl Ins {
    /// Shift every jump target by `base`: the lowering computes
    /// targets against its own per-driver instruction vector (base
    /// 0); linking addresses them absolutely in the concatenated
    /// program.
    pub(super) fn rebase_targets(&mut self, base: Ip) {
        let add = |to: &mut Ip| *to += base;
        match self {
            Ins::Jmp { to } => add(to),
            Ins::BrTruthy { to, .. } => add(to),
            Ins::TestTag { next_if_match, .. }
            | Ins::TestTagWide { next_if_match, .. }
            | Ins::TestBits { next_if_match, .. }
            | Ins::TestBit { next_if_match, .. } => add(next_if_match),
            _ => {}
        }
    }

    /// Check that every jump target of this instruction stays inside
    /// its own driver's `CodeRange` (the executor never crosses a
    /// driver boundary).
    pub(super) fn validate_targets(&self, range: CodeRange) {
        let check = |to: Ip| {
            assert!(
                to >= range.start && to < range.start + range.len,
                "jump target {to} outside range {}..{}",
                range.start,
                range.start + range.len
            );
        };
        match self {
            Ins::Jmp { to } => check(*to),
            Ins::BrTruthy { to, .. } => check(*to),
            Ins::TestTag { next_if_match, .. }
            | Ins::TestTagWide { next_if_match, .. }
            | Ins::TestBits { next_if_match, .. }
            | Ins::TestBit { next_if_match, .. } => check(*next_if_match),
            _ => {}
        }
    }

    /// Check that every slot reference in this instruction fits its
    /// frame layout.
    fn validate_slots(&self, frame: &FrameLayout) {
        let check_narrow = |slot: SlotN| {
            assert!((slot as usize) < frame.n_narrow as usize, "narrow slot {slot} out of frame");
        };
        let check_wide = |slot: SlotW| {
            assert!((slot as usize) < frame.n_wide as usize, "wide slot {slot} out of frame");
        };
        let check_dst = |dst: &Dst| match dst {
            Dst::Narrow(slot) => check_narrow(*slot),
            Dst::Wide(slot) => check_wide(*slot),
        };
        let check_src = |src: &Src| match src {
            Src::Narrow(slot) => check_narrow(*slot),
            Src::Wide(slot) => check_wide(*slot),
        };

        match self {
            Ins::Load { dst, .. } | Ins::Zero { dst, .. } => {
                check_dst(dst);
            }
            Ins::LoadUnresolved { dst, .. } => check_narrow(*dst),
            Ins::Const { dst, .. } => check_narrow(*dst),
            Ins::WideConst { dst, .. } => check_wide(*dst),
            Ins::Jmp { .. } => {}
            Ins::BrTruthy { cond, .. } => check_narrow(*cond),
            Ins::TestTag { subj, .. } => check_narrow(*subj),
            Ins::TestTagWide { subj, .. } => check_wide(*subj),
            Ins::Extract { subj, dst, .. } => {
                check_narrow(*subj);
                check_narrow(*dst);
            }
            Ins::ExtractWide { subj, dst, .. } => {
                check_wide(*subj);
                check_dst(dst);
            }
            Ins::TestBits { subj, .. } | Ins::TestBit { subj, .. } => check_narrow(*subj),
            Ins::Bin { lhs, rhs, dst, .. } => {
                check_narrow(*lhs);
                check_narrow(*rhs);
                check_narrow(*dst);
            }
            Ins::BinWide { op: _, lhs, rhs, dst } => {
                check_wide(*lhs);
                check_wide(*rhs);
                check_dst(dst);
            }
            Ins::Un { subj, dst, .. } => {
                check_narrow(*subj);
                check_narrow(*dst);
            }
            Ins::UnWide { subj, dst, .. } => {
                check_wide(*subj);
                check_dst(dst);
            }
            Ins::EncodeCtor { parts, dst, .. } => {
                for (slot, _) in parts.iter() {
                    check_narrow(*slot);
                }
                check_narrow(*dst);
            }
            Ins::EncodeCtorWide { args, dst, .. } => {
                for (src, _) in args.iter() {
                    check_src(src);
                }
                check_wide(*dst);
            }
            Ins::Splice { total_width: _, parts, dst } => {
                for (src, _, _) in parts.iter() {
                    check_src(src);
                }
                check_dst(dst);
            }
            Ins::Index { subj, dst, .. } | Ins::IndexRange { subj, dst, .. } => {
                check_narrow(*subj);
                check_narrow(*dst);
            }
            Ins::IndexWide { subj, .. } => check_wide(*subj),
            Ins::IndexDyn { subj, index, dst, .. } => {
                check_narrow(*subj);
                check_narrow(*index);
                check_narrow(*dst);
            }
            Ins::IndexDynWide { subj, index, .. } => {
                check_wide(*subj);
                check_narrow(*index);
            }
            Ins::IndexRangeWide { subj, dst, .. } => {
                check_wide(*subj);
                check_dst(dst);
            }
            Ins::Zext { subj, dst, .. } | Ins::Sext { subj, dst, .. } => {
                check_narrow(*subj);
                check_dst(dst);
            }
            Ins::ZextWide { subj, dst, .. } | Ins::SextWide { subj, dst, .. } => {
                check_wide(*subj);
                check_dst(dst);
            }
            Ins::Move { src, dst } => {
                check_narrow(*src);
                check_narrow(*dst);
            }
            Ins::MoveWide { src, dst } => {
                check_wide(*src);
                check_wide(*dst);
            }
            Ins::Any { subj, dst, .. } | Ins::All { subj, dst, .. } => {
                check_narrow(*subj);
                check_narrow(*dst);
            }
            Ins::AnyWide { subj, dst, .. } | Ins::AllWide { subj, dst, .. } => {
                check_wide(*subj);
                check_narrow(*dst);
            }
            Ins::Trap => {}
            Ins::Ret { src } => check_narrow(*src),
            Ins::RetWide { src } => check_wide(*src),
        }
        // Jump-target validity (within the driver's range) is checked
        // by the range walk in `Program::validate`.
    }
}
