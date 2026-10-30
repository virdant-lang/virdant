//! Execution: `Vm::run`, the bytecode interpreter driving one
//! compiled driver to its `Ret`/`RetWide`/`Trap`.
//!
//! Execution model: a plain loop over `ip` with one `match` arm per
//! instruction; the loop exits on `Ret`/`RetWide`/`Trap`, and the
//! control-flow forms set `ip` directly.
//!
//! `Load` is the only instruction that touches the engine's state
//! (raw bits out of `State`); `Ret` hands back raw result bits (the
//! bits-only commit path compares raw integers, `RunOut::into_value`
//! applies the `Program`'s per-driver `TypeId`/width tag) and
//! `RetWide` is the only place a `Value` is constructed.  Everything
//! between is bit arithmetic over the driver's frame: no allocation
//! on the 64-bit-and-narrower path.
//!
//! The `BinOp`/`UnOp` semantics below transcribe `eval.rs`'s
//! `eval_binop`/`eval_unop` exactly (see the checklist in
//! `vm/compile.rs`); where `eval.rs` compares raw `Value` bits, this
//! compares raw frame bits, which are the same bits masked to the
//! slot's static width.

use crate::analysis::elaboration::SignalId;
use crate::common::{BinOp, UnOp, Width};
use crate::sim::bits::BitPat;
use crate::sim::eval::Value;
use crate::sim::layout::TypeId;
use crate::sim::vm::frame::{Frame, Vm};
use crate::sim::vm::isa::{Dst, Ins, Program, Src, Truthiness};

/// What the executor reads out of engine state: the live value of a
/// signal, as raw bits (narrow) or a full pattern (wide).  `State`
/// implements it; the executor's unit tests use a table-backed stub.
pub(in crate::sim) trait VmState {
    /// The live value's low 64 bits (only read into narrow slots).
    fn narrow_bits(&self, signal: SignalId) -> u64;
    /// The live value's whole bit pattern (only read into wide slots).
    fn wide_bits(&self, signal: SignalId) -> BitPat;
}

/// `(1 << width) - 1` for `width < 64`; `u64::MAX` for `width >= 64`.
pub(in crate::sim) fn narrow_mask(width: u16) -> u64 {
    if width >= 64 { u64::MAX } else { (1u64 << width) - 1 }
}

/// Read `src`'s low bits out of the frame (wide slots truncated to
/// their low 64 bits, only used where the compiler proved the value
/// fits, or where the interpreter truncates the same way).
fn read(frame: &Frame, src: Src) -> u64 {
    match src {
        Src::Narrow(slot) => frame.narrow[slot as usize],
        Src::Wide(slot) => frame.wide[slot as usize].to_u64(),
    }
}

/// Read `src` as a `BitPat` of `width` bits: narrow slots are
/// materialized zero-padded, wide slots are cloned.
fn read_pat(frame: &Frame, src: Src, width: u16) -> BitPat {
    match src {
        Src::Narrow(slot) => BitPat::from_u64(width, frame.narrow[slot as usize]),
        Src::Wide(slot) => frame.wide[slot as usize].clone(),
    }
}

/// Write `bits` (a value of `width` bits) into `dst`, masking to the
/// destination slot's width and zero-extending/truncating as needed.
fn write(frame: &mut Frame, dst: Dst, bits: u64, width: u16) {
    match dst {
        Dst::Narrow(d) => frame.narrow[d as usize] = bits & narrow_mask(width),
        Dst::Wide(d) => {
            let slot_width = frame.wide[d as usize].width();
            let pat = BitPat::from_u64(width.min(64), bits);
            frame.wide[d as usize] = pat.zext_to(slot_width);
        }
    }
}

/// Write a whole `BitPat` result into `dst` (narrow results are
/// truncated to their low 64 bits, which only the compiler allows
/// when the destination is truly narrow).
fn write_pat(frame: &mut Frame, dst: Dst, pat: BitPat) {
    match dst {
        Dst::Narrow(d) => frame.narrow[d as usize] = pat.to_u64(),
        Dst::Wide(d) => frame.wide[d as usize] = pat,
    }
}

impl Vm {
    /// Execute the compiled driver of component `comp` to completion,
    /// reading live values out of `state`, and hand back the terminal
    /// result in its commit-shaped form (see [`RunOut`]).  The caller
    /// commits raw narrow bits through the bits-only path
    /// (`State::set_val_bits`/`set_reg_bits`, with the caller's
    /// clock-edge detection) and wide `Value`s through
    /// `State::set_val`/`set_reg`, exactly as it does for the
    /// interpreter.
    ///
    /// Panics when `comp` has no compiled driver (the caller must
    /// route fallback drivers to the interpreter), and on `Trap` —
    /// the lazy failure `Method`/`Fn`/`StrLit` evaluation has today.
    pub(in crate::sim) fn run_out<S: VmState>(&mut self, program: &Program, state: &S, comp: SignalId) -> RunOut {
        let idx = comp.index();
        let range = program.entry[idx]
            .unwrap_or_else(|| panic!("Vm::run_out: component {idx} has no compiled driver (must be routed to the interpreter fallback)"));
        let typ = program.driver_typs[idx]
            .unwrap_or_else(|| panic!("Vm::run_out: component {idx} has no compiled driver"));
        let ret_width = program.driver_widths[idx];
        let frame = &mut self.frames[idx];
        let mut ip = range.start as usize;
        loop {
            match &program.code[ip] {
                Ins::Load { signal, dst } => {
                    match dst {
                        Dst::Narrow(d) => frame.narrow[*d as usize] = state.narrow_bits(*signal),
                        Dst::Wide(d) => {
                            let pat = state.wide_bits(*signal);
                            frame.wide[*d as usize].set(&pat);
                        }
                    }
                    ip += 1;
                }
                Ins::LoadUnresolved { dst } => {
                    // `Context::get`'s fallback: a width-1 zero value.
                    frame.narrow[*dst as usize] = 0;
                    ip += 1;
                }
                Ins::Const { bits, dst } => {
                    frame.narrow[*dst as usize] = *bits;
                    ip += 1;
                }
                Ins::WideConst { bits, dst } => {
                    frame.wide[*dst as usize].set(bits);
                    ip += 1;
                }
                Ins::Zero { dst, width } => {
                    match dst {
                        Dst::Narrow(d) => frame.narrow[*d as usize] = 0,
                        Dst::Wide(d) => frame.wide[*d as usize] = BitPat::zeros(*width),
                    }
                    ip += 1;
                }
                Ins::Jmp { to } => {
                    ip = *to as usize;
                }
                Ins::BrTruthy { cond, kind, to } => {
                    let bits = frame.narrow[*cond as usize];
                    let truthy = match kind {
                        // `eval.rs::Expr::truthy`: a 1-bit value is true
                        // when its bit is set; anything wider is true
                        // when nonzero.
                        Truthiness::Bit => bits & 1 != 0,
                        Truthiness::NonZero => bits != 0,
                    };
                    ip = if truthy { *to as usize } else { ip + 1 };
                }
                Ins::TestTag { subj, tag_lo, tag_mask, tag, next_if_match } => {
                    let bits = frame.narrow[*subj as usize];
                    let hit = if (*tag_lo as usize) < 64 {
                        (bits >> tag_lo) & tag_mask == *tag
                    } else {
                        // Tag bits above the narrow slot's width are
                        // zero (slot-width invariant).
                        *tag == 0
                    };
                    ip = if hit { *next_if_match as usize } else { ip + 1 };
                }
                Ins::TestTagWide { subj, tag, next_if_match } => {
                    let pat = &frame.wide[*subj as usize];
                    let hit = tag.width() <= pat.width()
                        && (0..tag.width() as usize).all(|i| pat.get_bit(i) == tag.get_bit(i));
                    ip = if hit { *next_if_match as usize } else { ip + 1 };
                }
                Ins::Extract { subj, lo, width, dst } => {
                    let bits = frame.narrow[*subj as usize];
                    let out = if (*lo as usize) < 64 {
                        (bits >> lo) & narrow_mask(*width)
                    } else {
                        0
                    };
                    frame.narrow[*dst as usize] = out;
                    ip += 1;
                }
                Ins::ExtractWide { subj, lo, width, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    let sliced = pat.slice(*lo, lo.saturating_add(*width));
                    write_pat(frame, *dst, sliced);
                    ip += 1;
                }
                Ins::TestBits { subj, value, next_if_match } => {
                    let bits = frame.narrow[*subj as usize];
                    let hit = bits == *value;
                    ip = if hit { *next_if_match as usize } else { ip + 1 };
                }
                Ins::TestBit { subj, bit, next_if_match } => {
                    let bits = frame.narrow[*subj as usize];
                    let hit = (bits & 1 != 0) == *bit;
                    ip = if hit { *next_if_match as usize } else { ip + 1 };
                }
                Ins::Bin { op, width, lhs, rhs, dst } => {
                    let a = frame.narrow[*lhs as usize];
                    let b = frame.narrow[*rhs as usize];
                    frame.narrow[*dst as usize] = bin_narrow(*op, *width, a, b);
                    ip += 1;
                }
                Ins::BinWide { op, lhs, rhs, dst } => {
                    let a = frame.wide[*lhs as usize].clone();
                    let b = frame.wide[*rhs as usize].clone();
                    let out = bin_wide(*op, &a, &b);
                    write_pat(frame, *dst, out);
                    ip += 1;
                }
                Ins::Un { op, width, subj, dst } => {
                    let a = frame.narrow[*subj as usize];
                    frame.narrow[*dst as usize] = un_narrow(*op, *width, a);
                    ip += 1;
                }
                Ins::UnWide { op, subj, dst } => {
                    let a = frame.wide[*subj as usize].clone();
                    let out = match op {
                        UnOp::Not => BitPat::bit(a.is_zero()),
                        UnOp::Inv => a.not(),
                        UnOp::Neg => a.neg(),
                    };
                    write_pat(frame, *dst, out);
                    ip += 1;
                }
                Ins::EncodeCtor { total_width, tag, tag_lo, parts, dst } => {
                    // Transcribes `encode_ctor`: the tag in the low
                    // bits, each payload slot at its static bit
                    // offset.  Slot bits are zero above their own
                    // width (slot-width invariant), so plain shifts
                    // compose without extra masking; the final mask
                    // bounds the result to `total_width` (which also
                    // reproduces `zext_to`'s truncation for oversized
                    // payloads, as the interpreter does).
                    let mut bits = if (*tag_lo as usize) < 64 {
                        (*tag & narrow_mask(*total_width)) << tag_lo
                    } else {
                        0
                    };
                    for (slot, lo) in parts.iter() {
                        if (*lo as usize) < 64 {
                            bits |= frame.narrow[*slot as usize] << *lo;
                        }
                    }
                    frame.narrow[*dst as usize] = bits & narrow_mask(*total_width);
                    ip += 1;
                }
                Ins::EncodeCtorWide { typ, symbol, total_width, args, dst } => {
                    // Transcribes `encode_ctor` for wide types:
                    // delegates to the retained `TypeLayoutIndex::
                    // encode_ctor` on `BitPat`s (the wide path
                    // allocates, as the interpreter always did).
                    let arg_pats: Vec<BitPat> =
                        args.iter().map(|(src, w)| read_pat(frame, *src, *w)).collect();
                    let encoded = program
                        .layouts
                        .encode_ctor(*typ, *symbol, &arg_pats)
                        .unwrap_or_else(|| BitPat::zeros(*total_width));
                    frame.wide[*dst as usize] = encoded;
                    ip += 1;
                }
                Ins::Splice { total_width, parts, dst } => {
                    // Transcribes `eval_word`/`eval_struct`: MSB-first
                    // concatenation of the parts at static offsets,
                    // then zero-extension/truncation to `total_width`.
                    match dst {
                        Dst::Narrow(d) => {
                            let mut bits = 0u64;
                            for (src, lo, _) in parts.iter() {
                                if (*lo as usize) < 64 {
                                    bits |= read(frame, *src) << lo;
                                }
                            }
                            frame.narrow[*d as usize] = bits & narrow_mask(*total_width);
                        }
                        Dst::Wide(d) => {
                            let mut out = BitPat::zeros(*total_width);
                            for (src, lo, w) in parts.iter() {
                                let pat = read_pat(frame, *src, *w);
                                for i in 0..pat.width() as usize {
                                    if pat.get_bit(i) && *lo as usize + i < *total_width as usize {
                                        out.set_bit(*lo as usize + i, true);
                                    }
                                }
                            }
                            frame.wide[*d as usize] = out;
                        }
                    }
                    ip += 1;
                }
                Ins::Index { subj, index, dst } => {
                    // `Value::get_bit(index)`: false beyond the value's
                    // width; the slot's bits are zero there.
                    let bits = frame.narrow[*subj as usize];
                    let out = if (*index as usize) < 64 { (bits >> index) & 1 } else { 0 };
                    frame.narrow[*dst as usize] = out;
                    ip += 1;
                }
                Ins::IndexWide { subj, index, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    frame.narrow[*dst as usize] = pat.get_bit(*index as usize) as u64;
                    ip += 1;
                }
                Ins::IndexDyn { subj, index, dst } => {
                    let bits = frame.narrow[*subj as usize];
                    let idx = frame.narrow[*index as usize];
                    let out = if idx < 64 { (bits >> idx) & 1 } else { 0 };
                    frame.narrow[*dst as usize] = out;
                    ip += 1;
                }
                Ins::IndexDynWide { subj, index, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    let idx = frame.narrow[*index as usize];
                    frame.narrow[*dst as usize] = pat.get_bit(idx as usize) as u64;
                    ip += 1;
                }
                Ins::IndexRange { subj, lo, hi, dst } => {
                    // `x[hi..lo]`, `hi` exclusive: bits [lo, hi).
                    let bits = frame.narrow[*subj as usize];
                    let out = if (*lo as usize) < 64 {
                        (bits >> lo) & narrow_mask(hi.saturating_sub(*lo))
                    } else {
                        0
                    };
                    frame.narrow[*dst as usize] = out;
                    ip += 1;
                }
                Ins::IndexRangeWide { subj, lo, hi, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    let sliced = pat.slice(*lo, *hi);
                    write_pat(frame, *dst, sliced);
                    ip += 1;
                }
                Ins::Zext { subj, out_width, dst } => {
                    // `eval.rs::val_zext` narrow path: keep the low
                    // bits, mask/zero-extend to the node's type width.
                    let bits = frame.narrow[*subj as usize];
                    write(frame, *dst, bits, *out_width);
                    ip += 1;
                }
                Ins::ZextWide { subj, out_width, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    let out = pat.zext_to(*out_width);
                    write_pat(frame, *dst, out);
                    ip += 1;
                }
                Ins::Sext { subj, subj_width, out_width, dst } => {
                    let bits = frame.narrow[*subj as usize];
                    if *out_width <= 64 {
                        // Transcribes `eval_sext`'s Small path.
                        let msb = if *subj_width == 0 {
                            false
                        } else {
                            (bits >> (subj_width - 1)) & 1 != 0
                        };
                        let out = if *out_width >= *subj_width {
                            if msb {
                                bits | (narrow_mask(*out_width) & !narrow_mask(*subj_width))
                            } else {
                                bits & narrow_mask(*out_width)
                            }
                        } else {
                            bits & narrow_mask(*out_width)
                        };
                        write(frame, *dst, out, *out_width);
                    } else {
                        // Transcribes the Big path: `bits.sext_to(w)`.
                        let pat = BitPat::from_u64(*subj_width, bits);
                        write_pat(frame, *dst, pat.sext_to(*out_width));
                    }
                    ip += 1;
                }
                Ins::SextWide { subj, out_width, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    let out = pat.sext_to(*out_width);
                    write_pat(frame, *dst, out);
                    ip += 1;
                }
                Ins::Move { src, dst } => {
                    frame.narrow[*dst as usize] = frame.narrow[*src as usize];
                    ip += 1;
                }
                Ins::MoveWide { src, dst } => {
                    let pat = frame.wide[*src as usize].clone();
                    frame.wide[*dst as usize] = pat;
                    ip += 1;
                }
                Ins::Any { subj, dst } => {
                    let bits = frame.narrow[*subj as usize];
                    frame.narrow[*dst as usize] = (bits != 0) as u64;
                    ip += 1;
                }
                Ins::All { subj, width, dst } => {
                    let bits = frame.narrow[*subj as usize];
                    // NB: reproduces `Value::is_all_ones`'s small-value
                    // quirk, a 64-bit value is "all ones" regardless
                    // of its bits.
                    let out = *width == 64 || bits == narrow_mask(*width);
                    frame.narrow[*dst as usize] = out as u64;
                    ip += 1;
                }
                Ins::AnyWide { subj, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    frame.narrow[*dst as usize] = !pat.is_zero() as u64;
                    ip += 1;
                }
                Ins::AllWide { subj, dst } => {
                    let pat = &frame.wide[*subj as usize];
                    frame.narrow[*dst as usize] = pat.is_all_ones() as u64;
                    ip += 1;
                }
                Ins::Trap => unimplemented!(
                    "compiled driver reached a construct the lowering cannot represent \
                     (Method/Fn/StrLit), the same lazy failure the interpreter has"
                ),
                Ins::Ret { src } => {
                    // Masked to the driver's width, exactly as
                    // `Value::from_u64` masks: the bits-only commit
                    // path compares these raw against masked stored
                    // bits, so the masks must agree.
                    let bits = frame.narrow[*src as usize] & narrow_mask(ret_width);
                    return RunOut::Bits { typ, width: ret_width, bits };
                }
                Ins::RetWide { src } => {
                    let bits = frame.wide[*src as usize].clone();
                    return RunOut::Tagged(Value::new(typ, bits));
                }
            }
        }
    }
}

/// What one driver run produced, shaped for the commit paths
/// (PERF-IDEAS item 10): narrow (`Ret`) results carry raw bits plus
/// the tag a `Value` would carry — the bits-only commit path compares
/// raw integers and tags a `Value` only when it actually writes — and
/// already-tagged results stay `Value`s.
pub(in crate::sim) enum RunOut {
    /// A narrow (<= 64-bit) result: raw result bits (masked to
    /// `width`) plus the driver's `TypeId`/width, the tag
    /// [`Value::from_u64`] applies when a `Value` is finally needed.
    Bits { typ: TypeId, width: Width, bits: u64 },
    /// An already-tagged `Value`: a wide (`RetWide`) terminal result,
    /// or (at the `eval_driver` seam) a constant-unit skip or an
    /// interpreter-fallback result.
    Tagged(Value),
}

impl RunOut {
    /// The tagged `Value` this result carries: the old `Vm::run`'s
    /// exact result.
    pub(in crate::sim) fn into_value(self) -> Value {
        match self {
            RunOut::Bits { typ, width, bits } => Value::from_u64(typ, width, bits),
            RunOut::Tagged(value) => value,
        }
    }
}

/// Narrow binary op: bits in, bits out, no allocation.  Semantics
/// transcribe `eval.rs::eval_binop`'s Small path.
///
/// Note the two quirks kept from the interpreter:
/// * `LogicalAnd`/`LogicalOr`/`LogicalXor` look only at bit 0 of
///   their operands;
/// * comparisons treat the operand bits as zero-extended machine
///   words (same as the interpreter's `Small` path).
pub(super) fn bin_narrow(op: BinOp, width: u16, a: u64, b: u64) -> u64 {
    let m = narrow_mask(width);
    match op {
        BinOp::Add => a.wrapping_add(b) & m,
        BinOp::Sub => a.wrapping_sub(b) & m,
        BinOp::And => a & b,
        BinOp::Or => a | b,
        BinOp::Xor => a ^ b,
        BinOp::Eq => (a == b) as u64,
        BinOp::Neq => (a != b) as u64,
        BinOp::Lt => (a < b) as u64,
        BinOp::Lte => (a <= b) as u64,
        BinOp::Gt => (a > b) as u64,
        BinOp::Gte => (a >= b) as u64,
        BinOp::LogicalAnd => (((a & 1) != 0) && ((b & 1) != 0)) as u64,
        BinOp::LogicalOr => (((a & 1) != 0) || ((b & 1) != 0)) as u64,
        BinOp::LogicalXor => (((a & 1) != 0) ^ ((b & 1) != 0)) as u64,
    }
}

/// Wide binary op: delegates to the retained `BitPat` ops (the
/// interpreter's Big path).  Comparison/logical results are 1-bit.
pub(super) fn bin_wide(op: BinOp, a: &BitPat, b: &BitPat) -> BitPat {
    match op {
        BinOp::Add => a.add(b),
        BinOp::Sub => a.sub(b),
        BinOp::And => a.and(b),
        BinOp::Or => a.or(b),
        BinOp::Xor => a.xor(b),
        BinOp::Eq => BitPat::bit(a == b),
        BinOp::Neq => BitPat::bit(a != b),
        BinOp::Lt => BitPat::bit(a.ucmp(b) == std::cmp::Ordering::Less),
        BinOp::Lte => BitPat::bit(a.ucmp(b) != std::cmp::Ordering::Greater),
        BinOp::Gt => BitPat::bit(a.ucmp(b) == std::cmp::Ordering::Greater),
        BinOp::Gte => BitPat::bit(a.ucmp(b) != std::cmp::Ordering::Less),
        BinOp::LogicalAnd => BitPat::bit(a.get_bit(0) && b.get_bit(0)),
        BinOp::LogicalOr => BitPat::bit(a.get_bit(0) || b.get_bit(0)),
        BinOp::LogicalXor => BitPat::bit(a.get_bit(0) ^ b.get_bit(0)),
    }
}

/// Narrow unary op (transcribes `eval_unop`'s Small path; `Not`
/// yields a 1-bit result).
pub(super) fn un_narrow(op: UnOp, width: u16, a: u64) -> u64 {
    let m = narrow_mask(width);
    match op {
        UnOp::Neg => a.wrapping_neg() & m,
        UnOp::Inv => !a & m,
        UnOp::Not => (a == 0) as u64,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Width;
    use crate::sim::layout::{TypeInterner, TypeId, TypeLayoutIndex};
    use crate::types::Type;
    use crate::sim::vm::isa::{CodeRange, FrameLayout, Ip};

    /// A table-backed `VmState`: narrow bits per signal index (wide
    /// slots read as zero-width patterns, no test loads wide).
    struct TestState {
        narrow: Vec<u64>,
    }

    impl VmState for TestState {
        fn narrow_bits(&self, signal: SignalId) -> u64 {
            self.narrow.get(signal.index()).copied().unwrap_or(0)
        }
        fn wide_bits(&self, _signal: SignalId) -> BitPat {
            // No test loads into a wide slot through the stub.
            BitPat::zeros(0)
        }
    }

    /// Intern `typ` into a fresh layout index (no layouts needed by
    /// these programs, only the `Ret` type tag).
    fn interner_with(typ: &Type) -> (TypeLayoutIndex, TypeId) {
        let mut interner = TypeInterner::new();
        let id = interner.intern(typ);
        (TypeLayoutIndex::from_parts(Default::default(), interner), id)
    }

    fn run(ins: Vec<Ins>, ret: Ins, typ: &Type, ret_width: Width) -> Value {
        let (layouts, typ_id) = interner_with(typ);
        let mut code = ins;
        code.push(ret);
        let program = Program {
            code: code.into_boxed_slice(),
            entry: vec![Some(CodeRange { start: 0, len: 99 })],
            frames: vec![FrameLayout {
                n_narrow: 8,
                n_wide: 2,
                wide_widths: Box::new([128, 200]),
            }],
            driver_typs: vec![Some(typ_id)],
            driver_widths: vec![ret_width],
            layouts,
            const_values: vec![None],
            fallback_counts: [0; 3],
        };
        let mut program = program;
        program.entry[0] = Some(CodeRange { start: 0, len: program.code.len() as Ip });
        let mut vm = Vm::new(&program);
        vm.run_out(&program, &TestState { narrow: test_state() }, SignalId::from_index(0))
            .into_value()
    }

    fn test_state() -> Vec<u64> {
        // signal 0: 0b1010, signal 1: 0b0011, signal 2: all ones of 8 bits
        vec![0b1010, 0b0011, 0b1111_1111, 0, 0, 0, 0, 0]
    }

    #[test]
    fn const_and_bin_add_masks_to_width() {
        let value = run(
            vec![
                Ins::Const { bits: 0b0111, dst: 0 },
                Ins::Const { bits: 0b1001, dst: 1 },
                Ins::Bin {
                    op: BinOp::Add,
                    width: 4,
                    lhs: 0,
                    rhs: 1,
                    dst: 2,
                },
            ],
            Ins::Ret { src: 2 },
            &Type::Word(4),
            4,
        );
        // 0b0111 + 0b1001 = 0b1_0000 -> masked to 4 bits: 0.
        assert_eq!(value.to_u64(), 0);
        assert_eq!(value.width(), 4);
    }

    #[test]
    fn load_reads_live_state_bits() {
        let value = run(
            vec![Ins::Load {
                signal: SignalId::from_index(0),
                dst: Dst::Narrow(0),
            }],
            Ins::Ret { src: 0 },
            &Type::Word(4),
            4,
        );
        assert_eq!(value.to_u64(), 0b1010);
        assert_eq!(value.width(), 4);
    }

    #[test]
    fn bin_comparisons_and_logical_ops_read_bit0_only() {
        // (op, a, b, expected, result width: comparisons/logical are
        // 1-bit; bitwise ops keep the operand width)
        let cases: Vec<(BinOp, u64, u64, u64, Width)> = vec![
            (BinOp::Eq, 0b1010, 0b1010, 1, 1),
            (BinOp::Neq, 0b1010, 0b0011, 1, 1),
            (BinOp::Lt, 0b0011, 0b1010, 1, 1),
            (BinOp::Gte, 0b1010, 0b0011, 1, 1),
            (BinOp::LogicalAnd, 0b1, 0b1, 1, 1),
            (BinOp::LogicalAnd, 0b10, 0b1, 0, 1), // bit 0 of 0b10 is 0
            (BinOp::LogicalOr, 0b10, 0b0, 0, 1),
            (BinOp::LogicalXor, 0b1, 0b0, 1, 1),
            (BinOp::And, 0b1100, 0b1010, 0b1000, 4),
            (BinOp::Or, 0b1100, 0b0010, 0b1110, 4),
            (BinOp::Xor, 0b1100, 0b1010, 0b0110, 4),
        ];
        for (i, (op, a, b, expected, result_width)) in cases.iter().enumerate() {
            let value = run(
                vec![
                    Ins::Const { bits: *a, dst: 0 },
                    Ins::Const { bits: *b, dst: 1 },
                    Ins::Bin {
                        op: *op,
                        width: 4,
                        lhs: 0,
                        rhs: 1,
                        dst: 2,
                    },
                ],
                Ins::Ret { src: 2 },
                &Type::Bit,
                *result_width,
            );
            assert_eq!(value.to_u64(), *expected, "case {i}: {op:?}");
        }
    }

    #[test]
    fn un_ops_neg_inv_not() {
        // Neg on width 8: 0 - 5 = 251.
        let value = run(
            vec![
                Ins::Const { bits: 5, dst: 0 },
                Ins::Un {
                    op: UnOp::Neg,
                    width: 8,
                    subj: 0,
                    dst: 1,
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Word(8),
            8,
        );
        assert_eq!(value.to_u64(), 251);

        // Inv on width 8: !5 = 250.
        let value = run(
            vec![
                Ins::Const { bits: 5, dst: 0 },
                Ins::Un {
                    op: UnOp::Inv,
                    width: 8,
                    subj: 0,
                    dst: 1,
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Word(8),
            8,
        );
        assert_eq!(value.to_u64(), 250);

        // Not: zero -> 1.
        let value = run(
            vec![
                Ins::Const { bits: 0, dst: 0 },
                Ins::Un {
                    op: UnOp::Not,
                    width: 8,
                    subj: 0,
                    dst: 1,
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Bit,
            1,
        );
        assert_eq!(value.to_u64(), 1);
    }

    #[test]
    fn sext_replicates_the_msb() {
        // Subject: 0b1000 (4 bits), sext to 8: 0b1111_1000.
        let value = run(
            vec![
                Ins::Const { bits: 0b1000, dst: 0 },
                Ins::Sext {
                    subj: 0,
                    subj_width: 4,
                    out_width: 8,
                    dst: Dst::Narrow(1),
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Word(8),
            8,
        );
        assert_eq!(value.to_u64(), 0b1111_1000);
        assert_eq!(value.width(), 8);
    }

    #[test]
    fn zext_truncates_and_extends() {
        // Trunc semantics: low bits kept, masked to out width.
        let value = run(
            vec![
                Ins::Const { bits: 0b1_0101, dst: 0 },
                Ins::Zext {
                    subj: 0,
                    out_width: 3,
                    dst: Dst::Narrow(1),
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Word(3),
            3,
        );
        assert_eq!(value.to_u64(), 0b101);
    }

    #[test]
    fn index_and_index_range() {
        // x[2] of 0b1010 -> 0; x[1] -> 1.
        for (index, expected) in [(2u16, 0u64), (1, 1)] {
            let value = run(
                vec![
                    Ins::Const { bits: 0b1010, dst: 0 },
                    Ins::Index {
                        subj: 0,
                        index,
                        dst: 1,
                    },
                ],
                Ins::Ret { src: 1 },
                &Type::Bit,
                1,
            );
            assert_eq!(value.to_u64(), expected, "x[{index}]");
        }
        // Index past the value's width reads zero (masked bits).
        let value = run(
            vec![
                Ins::Const { bits: 0b1010, dst: 0 },
                Ins::Index {
                    subj: 0,
                    index: 5,
                    dst: 1,
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Bit,
            1,
        );
        assert_eq!(value.to_u64(), 0);

        // x[4..1] of 0b1010 -> bits 1..4 = 0b101.
        let value = run(
            vec![
                Ins::Const { bits: 0b1010, dst: 0 },
                Ins::IndexRange {
                    subj: 0,
                    lo: 1,
                    hi: 4,
                    dst: 1,
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Word(3),
            3,
        );
        assert_eq!(value.to_u64(), 0b101);
    }

    #[test]
    fn index_dyn_and_index_dyn_of_dynamic_zero() {
        let value = run(
            vec![
                Ins::Const { bits: 0b0011, dst: 1 },
                Ins::Const { bits: 1, dst: 0 },
                Ins::IndexDyn {
                    subj: 1,
                    index: 0,
                    dst: 2,
                },
            ],
            Ins::Ret { src: 2 },
            &Type::Bit,
            1,
        );
        assert_eq!(value.to_u64(), 1);

        // Index 64 reads 0 (no shift overflow).
        let value = run(
            vec![
                Ins::Const { bits: 0b0011, dst: 1 },
                Ins::Const { bits: 64, dst: 0 },
                Ins::IndexDyn {
                    subj: 1,
                    index: 0,
                    dst: 2,
                },
            ],
            Ins::Ret { src: 2 },
            &Type::Bit,
            1,
        );
        assert_eq!(value.to_u64(), 0);
    }

    #[test]
    fn any_all_reductions() {
        let value = run(
            vec![
                Ins::Const { bits: 0b1010, dst: 0 },
                Ins::Any { subj: 0, dst: 1 },
            ],
            Ins::Ret { src: 1 },
            &Type::Bit,
            1,
        );
        assert_eq!(value.to_u64(), 1); // 0b1010 != 0

        // All over width-4 all-ones.
        let value = run(
            vec![
                Ins::Const { bits: 0b1111, dst: 0 },
                Ins::All {
                    subj: 0,
                    width: 4,
                    dst: 1,
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Bit,
            1,
        );
        assert_eq!(value.to_u64(), 1);
    }

    #[test]
    fn splice_concatenates_msb_first() {
        // {a, b} with a = 0b101 (3 bits), b = 0b11 (2 bits) into
        // total width 5: bits 4..3 = 101, bits 2..0 = 11 -> 0b10111.
        let value = run(
            vec![
                Ins::Const { bits: 0b101, dst: 0 },
                Ins::Const { bits: 0b11, dst: 1 },
                Ins::Splice {
                    total_width: 5,
                    parts: Box::new([(Src::Narrow(0), 2, 3), (Src::Narrow(1), 0, 2)]),
                    dst: Dst::Narrow(2),
                },
            ],
            Ins::Ret { src: 2 },
            &Type::Word(5),
            5,
        );
        assert_eq!(value.to_u64(), 0b10111);
    }

    #[test]
    fn encode_ctor_places_tag_and_payload() {
        // tag = 0b10 (one-hot ctor 1) at bits [0,2), payload 0b1101
        // (4 bits) at bits [2,6): 0b1101_10.
        let value = run(
            vec![
                Ins::Const { bits: 0b1101, dst: 0 },
                Ins::EncodeCtor {
                    total_width: 6,
                    tag: 0b10,
                    tag_lo: 0,
                    parts: Box::new([(0, 2)]),
                    dst: 1,
                },
            ],
            Ins::Ret { src: 1 },
            &Type::Word(6),
            6,
        );
        assert_eq!(value.to_u64(), 0b1101_10);
    }

    #[test]
    fn zero_and_load_unresolved() {
        let value = run(
            vec![
                Ins::Zero {
                    dst: Dst::Narrow(0),
                    width: 4,
                },
            ],
            Ins::Ret { src: 0 },
            &Type::Word(4),
            4,
        );
        assert_eq!(value.to_u64(), 0);

        let value = run(
            vec![Ins::LoadUnresolved { dst: 0 }],
            Ins::Ret { src: 0 },
            &Type::Bit,
            1,
        );
        assert_eq!(value.to_u64(), 0);
        assert_eq!(value.width(), 1);
    }

    #[test]
    fn ret_attaches_the_driver_type_and_width() {
        let (layouts, typ_id) = interner_with(&Type::Word(4));
        let program = Program {
            code: vec![
                Ins::Const { bits: 0b1010, dst: 0 },
                Ins::Ret { src: 0 },
            ]
            .into_boxed_slice(),
            entry: vec![Some(CodeRange { start: 0, len: 2 })],
            frames: vec![FrameLayout {
                n_narrow: 8,
                n_wide: 2,
                wide_widths: Box::new([128, 200]),
            }],
            driver_typs: vec![Some(typ_id)],
            driver_widths: vec![4],
            layouts,
            const_values: vec![None],
            fallback_counts: [0; 3],
        };
        let mut vm = Vm::new(&program);
        let value = vm.run_out(
            &program,
            &TestState { narrow: test_state() },
            SignalId::from_index(0),
        );
        match value {
            RunOut::Bits { typ, width, bits } => {
                assert_eq!(typ, typ_id);
                assert_eq!(width, 4);
                assert_eq!(bits, 0b1010);
                // `into_value` tags the raw bits exactly as `Ret` used to.
                let value = RunOut::Bits { typ, width, bits }.into_value();
                assert_eq!(value.typ_id(), typ_id);
                assert_eq!(value.width(), 4);
                assert_eq!(value.to_u64(), 0b1010);
            }
            RunOut::Tagged(_) => panic!("narrow driver must hand back raw bits"),
        }
    }

    #[test]
    fn wide_ret_returns_the_full_pattern() {
        let (layouts, typ_id) = interner_with(&Type::Word(100));
        let mut pattern = BitPat::zeros(100);
        pattern.set_bit(70, true);
        pattern.set_bit(3, true);
        let program = Program {
            code: vec![
                Ins::WideConst { bits: pattern.clone(), dst: 0 },
                Ins::RetWide { src: 0 },
            ]
            .into_boxed_slice(),
            entry: vec![Some(CodeRange { start: 0, len: 2 })],
            frames: vec![FrameLayout {
                n_narrow: 8,
                n_wide: 2,
                wide_widths: Box::new([100, 200]),
            }],
            driver_typs: vec![Some(typ_id)],
            driver_widths: vec![100],
            layouts,
            const_values: vec![None],
            fallback_counts: [0; 3],
        };
        let mut vm = Vm::new(&program);
        let out = vm.run_out(
            &program,
            &TestState { narrow: test_state() },
            SignalId::from_index(0),
        );
        // Wide drivers stay on the `Value` path: the terminal result
        // is already the tagged `Value`.
        let value = match out {
            RunOut::Tagged(value) => value,
            RunOut::Bits { .. } => panic!("wide driver must not hand back raw bits"),
        };
        assert_eq!(value.width(), 100);
        assert!(value.get_bit(70));
        assert!(value.get_bit(3));
        assert!(!value.get_bit(69));
    }

    #[test]
    #[should_panic]
    fn trap_panics_lazily() {
        let value = run(
            vec![Ins::Trap],
            Ins::Ret { src: 0 },
            &Type::Bit,
            1,
        );
        let _ = value;
    }

    #[test]
    #[should_panic(expected = "no compiled driver")]
    fn run_panics_on_fallback_drivers() {
        let (layouts, _typ_id) = interner_with(&Type::Bit);
        let program = Program {
            code: Vec::new().into_boxed_slice(),
            entry: vec![None],
            frames: vec![FrameLayout::default()],
            driver_typs: vec![None],
            driver_widths: vec![0],
            layouts,
            const_values: vec![None],
            fallback_counts: [1, 0, 0],
        };
        let mut vm = Vm::new(&program);
        let _ = vm.run_out(
            &program,
            &TestState { narrow: test_state() },
            SignalId::from_index(0),
        );
    }
}
