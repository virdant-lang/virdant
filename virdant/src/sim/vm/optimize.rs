//! Optimizer passes over the emitted `Ins` stream, run per driver
//! (inside one `CodeRange`, never across drivers) between lowering
//! and link.  See DESIGN.md "Compilation": passes rewrite `Ins`
//! sequences within the existing opcode set, so the emitted format is
//! unchanged.
//!
//! Pipeline per driver (iterated to a fixpoint):
//!   1. `fold_constants`: fold `Bin`/`Un`/`Extract`/`Index`/etc. over
//!      `Const` operands; delete `BrTruthy`/`Jmp` with constant
//!      conditions and select the arm of a `Match` with a constant
//!      subject; fold `EncodeCtor`/`Splice` over constant args.
//!   2. `cse`: eliminate redundant computes within a driver.
//!   3. `slot_fusion`: fuse single-use producer->consumer chains.
//!   4. `clean`: remove no-op jumps, unreachable code (UCE) and dead
//!      writes (backward liveness).
//!
//! All passes work on the same `Vec<Ins>` the lowering emitted; there
//! is no separate pre-link IR.  Cross-driver analysis stays
//! documented headroom (DESIGN.md).

use std::collections::HashMap;

use crate::common::UnOp;
use crate::sim::bits::BitPat;
use crate::sim::vm::isa::{Dst, Ins, Ip, SlotN, SlotW, Src, Truthiness};

/// Run the optimization pipeline over one driver's `Ins` sequence
/// (the whole vector is one driver's code range at this stage).
pub(in crate::sim) fn optimize_driver(code: &mut Vec<Ins>) {
    // Iterate to a fixpoint: each pass enables the next (folding
    // exposes CSE, CSE exposes fusion, clean exposes folding).
    for _ in 0..8 {
        let before = code.len();
        fold_constants(code);
        cse(code);
        slot_fusion(code);
        clean(code);
        if code.len() == before {
            break;
        }
    }
}

// ---------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------

/// `2^width - 1` for `width <= 64`, `u64::MAX` above (matching the
/// executor's `narrow_mask`).
fn mask(width: u16) -> u64 {
    if width == 0 {
        0
    } else if width >= 64 {
        u64::MAX
    } else {
        (1u64 << width) - 1
    }
}

/// Every ip this instruction can transfer control to (explicit
/// targets only; the implicit fall-through is handled by callers).
fn jump_targets(ins: &Ins) -> Vec<Ip> {
    match ins {
        Ins::Jmp { to } => vec![*to],
        Ins::BrTruthy { to, .. } => vec![*to],
        Ins::TestTag { next_if_match, .. }
        | Ins::TestTagWide { next_if_match, .. }
        | Ins::TestBits { next_if_match, .. }
        | Ins::TestBit { next_if_match, .. } => vec![*next_if_match],
        _ => Vec::new(),
    }
}

/// Whether the instruction is control flow or a terminator (control
/// never falls through it, and its position is not a straight-line
/// step).
fn is_control(ins: &Ins) -> bool {
    matches!(
        ins,
        Ins::Jmp { .. }
            | Ins::BrTruthy { .. }
            | Ins::TestTag { .. }
            | Ins::TestTagWide { .. }
            | Ins::TestBits { .. }
            | Ins::TestBit { .. }
            | Ins::Trap
            | Ins::Ret { .. }
            | Ins::RetWide { .. }
    )
}

/// The slot this instruction writes, if any.
fn def_dst(ins: &Ins) -> Option<Dst> {
    match ins {
        // `Dst` destinations (may be narrow or wide).
        Ins::Load { dst, .. }
        | Ins::LoadWord { dst, .. }
        | Ins::LoadWordDyn { dst, .. }
        | Ins::Zero { dst, .. }
        | Ins::ExtractWide { dst, .. }
        | Ins::BinWide { dst, .. }
        | Ins::UnWide { dst, .. }
        | Ins::Splice { dst, .. }
        | Ins::IndexRangeWide { dst, .. }
        | Ins::Zext { dst, .. }
        | Ins::ZextWide { dst, .. }
        | Ins::Sext { dst, .. }
        | Ins::SextWide { dst, .. } => Some(*dst),
        // Narrow destinations.
        Ins::LoadUnresolved { dst }
        | Ins::Const { dst, .. }
        | Ins::Extract { dst, .. }
        | Ins::Index { dst, .. }
        | Ins::IndexRange { dst, .. }
        | Ins::Bin { dst, .. }
        | Ins::Un { dst, .. }
        | Ins::Move { dst, .. }
        | Ins::Any { dst, .. }
        | Ins::All { dst, .. }
        | Ins::IndexWide { dst, .. }
        | Ins::IndexDyn { dst, .. }
        | Ins::IndexDynWide { dst, .. }
        | Ins::AnyWide { dst, .. }
        | Ins::AllWide { dst, .. }
        | Ins::EncodeCtor { dst, .. } => Some(Dst::Narrow(*dst)),
        // Wide destinations.
        Ins::WideConst { dst, .. }
        | Ins::EncodeCtorWide { dst, .. }
        | Ins::MoveWide { dst, .. } => Some(Dst::Wide(*dst)),
        Ins::Jmp { .. }
        | Ins::BrTruthy { .. }
        | Ins::TestTag { .. }
        | Ins::TestTagWide { .. }
        | Ins::TestBits { .. }
        | Ins::TestBit { .. }
        | Ins::Trap
        | Ins::Ret { .. }
        | Ins::RetWide { .. } => None,
    }
}

/// Visit every slot this instruction *reads* (narrow slots as
/// `Dst::Narrow`, wide as `Dst::Wide`, so one visitor covers both).
fn for_each_slot(ins: &Ins, f: &mut dyn FnMut(Dst)) {
    macro_rules! n {
        ($s:expr) => {
            f(Dst::Narrow(*$s))
        };
    }
    macro_rules! w {
        ($s:expr) => {
            f(Dst::Wide(*$s))
        };
    }
    match ins {
        Ins::Jmp { .. } | Ins::Trap => {}
        Ins::BrTruthy { cond, .. } => n!(cond),
        Ins::TestTag { subj, .. } => n!(subj),
        Ins::TestTagWide { subj, .. } => w!(subj),
        Ins::Extract { subj, .. } => n!(subj),
        Ins::ExtractWide { subj, .. } => w!(subj),
        Ins::TestBits { subj, .. } | Ins::TestBit { subj, .. } => n!(subj),
        Ins::Bin { lhs, rhs, .. } => {
            n!(lhs);
            n!(rhs);
        }
        Ins::BinWide { lhs, rhs, .. } => {
            w!(lhs);
            w!(rhs);
        }
        Ins::Un { subj, .. } => n!(subj),
        Ins::UnWide { subj, .. } => w!(subj),
        Ins::EncodeCtor { parts, .. } => {
            for (slot, _) in parts.iter() {
                n!(slot);
            }
        }
        Ins::EncodeCtorWide { args, .. } => {
            for (src, _) in args.iter() {
                src_visit(*src, f);
            }
        }
        Ins::Splice { parts, .. } => {
            for (src, _, _) in parts.iter() {
                src_visit(*src, f);
            }
        }
        Ins::Index { subj, .. } | Ins::IndexRange { subj, .. } => n!(subj),
        Ins::IndexWide { subj, .. } => w!(subj),
        Ins::IndexDyn { subj, index, .. } => {
            n!(subj);
            n!(index);
        }
        Ins::IndexDynWide { subj, index, .. } => {
            w!(subj);
            n!(index);
        }
        Ins::IndexRangeWide { subj, .. } => w!(subj),
        Ins::Zext { subj, .. } | Ins::Sext { subj, .. } => n!(subj),
        Ins::ZextWide { subj, .. } | Ins::SextWide { subj, .. } => w!(subj),
        Ins::Move { src, .. } => n!(src),
        Ins::MoveWide { src, .. } => w!(src),
        Ins::Any { subj, .. } | Ins::All { subj, .. } => n!(subj),
        Ins::AnyWide { subj, .. } | Ins::AllWide { subj, .. } => w!(subj),
        // `Load`/`LoadWord` read state, not frame slots;
        // `LoadWordDyn` reads its index slot.
        Ins::Load { .. }
        | Ins::LoadWord { .. }
        | Ins::LoadUnresolved { .. } => {}
        Ins::LoadWordDyn { index, .. } => n!(index),
        Ins::Const { .. } | Ins::WideConst { .. } | Ins::Zero { .. } => {}
        Ins::Ret { src } => n!(src),
        Ins::RetWide { src } => w!(src),
    }
}

fn src_visit(src: Src, f: &mut dyn FnMut(Dst)) {
    match src {
        Src::Narrow(s) => f(Dst::Narrow(s)),
        Src::Wide(s) => f(Dst::Wide(s)),
    }
}

/// Frame extent: the largest narrow and wide slot indices this code
/// uses (writes or reads), for sizing the liveness bit vectors.
fn frame_extent(code: &[Ins]) -> (u16, u16) {
    let mut max_narrow: i64 = -1;
    let mut max_wide: i64 = -1;
    for ins in code.iter() {
        for_each_slot(ins, &mut |dst| match dst {
            Dst::Narrow(s) => max_narrow = max_narrow.max(s as i64),
            Dst::Wide(s) => max_wide = max_wide.max(s as i64),
        });
        if let Some(dst) = def_dst(ins) {
            match dst {
                Dst::Narrow(s) => max_narrow = max_narrow.max(s as i64),
                Dst::Wide(s) => max_wide = max_wide.max(s as i64),
            }
        }
    }
    ((max_narrow + 1).max(0) as u16, (max_wide + 1).max(0) as u16)
}

fn slot_index(slot: Dst, n_narrow: u16) -> usize {
    match slot {
        Dst::Narrow(s) => s as usize,
        Dst::Wide(s) => n_narrow as usize + s as usize,
    }
}

fn is_live(dst: Dst, live: &[bool], n_narrow: u16) -> bool {
    let idx = slot_index(dst, n_narrow);
    idx < live.len() && live[idx]
}

/// Drop the instructions flagged dead and remap the surviving jump
/// targets.  A removed instruction's control flow lands on the next
/// kept instruction (control falls through removed non-terminators).
fn rebuild(code: &mut Vec<Ins>, dead: &[bool]) {
    let len = code.len();
    debug_assert_eq!(dead.len(), len);
    // map[old] = the new ip of the instruction that control reaching
    // old ip lands on: the old instruction's own new position if it is
    // kept, otherwise the next kept instruction's new position.
    let mut map: Vec<Ip> = vec![0; len];
    let mut new_len: Ip = 0;
    for i in 0..len {
        if !dead[i] {
            map[i] = new_len;
            new_len += 1;
        }
    }
    let mut next_new = new_len;
    for i in (0..len).rev() {
        if dead[i] {
            map[i] = next_new;
        } else {
            next_new = map[i];
        }
    }
    let mut kept: Vec<Ins> = Vec::with_capacity(len);
    for (i, ins) in code.iter().enumerate() {
        if dead[i] {
            continue;
        }
        let mut ins = ins.clone();
        retarget(&mut ins, &map);
        kept.push(ins);
    }
    *code = kept;
}

fn retarget(ins: &mut Ins, map: &[Ip]) {
    match ins {
        Ins::Jmp { to } => *to = map[*to as usize],
        Ins::BrTruthy { to, .. } => *to = map[*to as usize],
        Ins::TestTag { next_if_match, .. }
        | Ins::TestTagWide { next_if_match, .. }
        | Ins::TestBits { next_if_match, .. }
        | Ins::TestBit { next_if_match, .. } => *next_if_match = map[*next_if_match as usize],
        _ => {}
    }
}

// ---------------------------------------------------------------------
// Pass 1: constant folding and branch propagation
// ---------------------------------------------------------------------

/// How the scan arrived at the current ip:
/// * `Fresh`: by fall-through from the previous (scanned) instruction;
///   the map is valid unless another live edge joins here.
/// * `Edge`: by a taken/propagated edge the scan followed through a
///   skipped dead region or a no-op jump; the map is valid iff this is
///   the only in-edge (`jump_preds == 1`).
/// * `Walk`: entering a dead region the scan must traverse anyway (or
///   resuming after a terminator); the map is invalid, clear it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Arrival {
    Fresh,
    Edge,
    Walk,
}

/// Fold constants and propagate branches.  Rewrites compute
/// instructions with all-constant operands into `Const`/`WideConst`,
/// rewrites constant `Move`s, and deletes constant-condition branches
/// by rewriting them to unconditional `Jmp`s (the dead path is then
/// removed by `clean`'s UCE).
fn fold_constants(code: &mut Vec<Ins>) {
    let len = code.len();
    if len == 0 {
        return;
    }
    // In-edge counts per ip (explicit jump edges only; fall-through
    // edges are implicit and checked separately).
    let mut jump_preds: Vec<u32> = vec![0; len];
    for ins in code.iter() {
        for t in jump_targets(ins) {
            let t = t as usize;
            if t < len {
                jump_preds[t] += 1;
            }
        }
    }
    let is_target: Vec<bool> = jump_preds.iter().map(|&p| p > 0).collect();

    // Constant environment per slot class.
    let mut cn: HashMap<SlotN, u64> = HashMap::new();
    let mut cw: HashMap<SlotW, BitPat> = HashMap::new();
    let mut arrival = Arrival::Fresh;
    let mut ip: usize = 0;
    while ip < len {
        // Join points invalidate the environment.
        if arrival == Arrival::Walk
            || (arrival == Arrival::Fresh && is_target[ip])
            || (arrival == Arrival::Edge && jump_preds[ip] != 1)
        {
            cn.clear();
            cw.clear();
        }
        let ins = code[ip].clone();
        let next = fold_one(
            code,
            ip,
            &ins,
            &jump_preds,
            &mut cn,
            &mut cw,
            &mut arrival,
        );
        ip = next;
    }
}

/// Fold one instruction at `ip`, rewriting `code[ip]` in place when a
/// fold applies, and returning the next ip to scan with the updated
/// arrival.
#[allow(clippy::too_many_arguments)]
fn fold_one(
    code: &mut Vec<Ins>,
    ip: usize,
    ins: &Ins,
    jump_preds: &[u32],
    cn: &mut HashMap<SlotN, u64>,
    cw: &mut HashMap<SlotW, BitPat>,
    arrival: &mut Arrival,
) -> usize {
    macro_rules! n_const {
        ($slot:expr) => {
            cn.get(&$slot).copied()
        };
    }
    macro_rules! w_const {
        ($slot:expr) => {
            cw.get(&$slot).cloned()
        };
    }
    match ins {
        Ins::Load { dst, .. } | Ins::LoadWord { dst, .. } | Ins::LoadWordDyn { dst, .. } => {
            invalidate_dst(*dst, cn, cw);
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::LoadUnresolved { dst } => {
            // The executor writes a width-1 zero here, so the fold is
            // exact.
            code[ip] = Ins::Const { bits: 0, dst: *dst };
            cn.insert(*dst, 0);
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Const { bits, dst } => {
            cn.insert(*dst, *bits);
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::WideConst { bits, dst } => {
            cw.insert(*dst, bits.clone());
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Zero { dst, width } => {
            match *dst {
                Dst::Narrow(d) => {
                    cn.insert(d, 0);
                }
                Dst::Wide(d) => {
                    cw.insert(d, BitPat::zeros(*width));
                }
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Jmp { to } => {
            let to = *to as usize;
            if to <= ip {
                // A backward jump would loop the scan; drivers have no
                // loops, so treat the scan defensively and step past.
                *arrival = Arrival::Walk;
                return ip + 1;
            }
            *arrival = if to == ip + 1 { Arrival::Edge } else { Arrival::Walk };
            to
        }
        Ins::BrTruthy { cond, kind, to } => {
            handle_branch(
                code,
                ip,
                n_const!(*cond).map(|a| match kind {
                    Truthiness::Bit => a & 1 != 0,
                    Truthiness::NonZero => a != 0,
                }),
                *to as usize,
                jump_preds,
                arrival,
            )
        }
        Ins::TestTag { subj, tag_lo, tag_mask, tag, next_if_match } => {
            handle_branch(
                code,
                ip,
                n_const!(*subj).map(|a| {
                    if (*tag_lo as usize) < 64 {
                        (a >> tag_lo) & tag_mask == *tag
                    } else {
                        *tag == 0
                    }
                }),
                *next_if_match as usize,
                jump_preds,
                arrival,
            )
        }
        Ins::TestTagWide { subj, tag, next_if_match } => {
            handle_branch(
                code,
                ip,
                w_const!(*subj).map(|pat| {
                    tag.width() <= pat.width()
                        && (0..tag.width() as usize).all(|i| pat.get_bit(i) == tag.get_bit(i))
                }),
                *next_if_match as usize,
                jump_preds,
                arrival,
            )
        }
        Ins::TestBits { subj, value, next_if_match } => {
            handle_branch(
                code,
                ip,
                n_const!(*subj).map(|a| a == *value),
                *next_if_match as usize,
                jump_preds,
                arrival,
            )
        }
        Ins::TestBit { subj, bit, next_if_match } => {
            handle_branch(
                code,
                ip,
                n_const!(*subj).map(|a| (a & 1 != 0) == *bit),
                *next_if_match as usize,
                jump_preds,
                arrival,
            )
        }
        Ins::Extract { subj, lo, width, dst } => {
            if let Some(a) = n_const!(*subj) {
                let out = if (*lo as usize) < 64 {
                    (a >> lo) & mask(*width)
                } else {
                    0
                };
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::ExtractWide { subj, lo, width, dst } => {
            if let Some(pat) = w_const!(*subj) {
                let sliced = pat.slice(*lo, lo.saturating_add(*width));
                write_const_dst(*dst, &sliced, cn, cw);
                code[ip] = dst_ins(*dst, &sliced);
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Bin { op, width, lhs, rhs, dst } => {
            if let (Some(a), Some(b)) = (n_const!(*lhs), n_const!(*rhs)) {
                let out = crate::sim::vm::exec::bin_narrow(*op, *width, a, b);
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::BinWide { op, lhs, rhs, dst } => {
            if let (Some(a), Some(b)) = (w_const!(*lhs), w_const!(*rhs)) {
                let out = crate::sim::vm::exec::bin_wide(*op, &a, &b);
                write_const_dst(*dst, &out, cn, cw);
                code[ip] = dst_ins(*dst, &out);
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Un { op, width, subj, dst } => {
            if let Some(a) = n_const!(*subj) {
                let out = crate::sim::vm::exec::un_narrow(*op, *width, a);
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::UnWide { op, subj, dst } => {
            if let Some(a) = w_const!(*subj) {
                let out = match op {
                    UnOp::Not => BitPat::bit(a.is_zero()),
                    UnOp::Inv => a.not(),
                    UnOp::Neg => a.neg(),
                };
                write_const_dst(*dst, &out, cn, cw);
                code[ip] = dst_ins(*dst, &out);
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::EncodeCtor { total_width, tag, tag_lo, parts, dst } => {
            let all_const = parts.iter().all(|(slot, _)| cn.contains_key(slot));
            if all_const {
                let mut bits = if (*tag_lo as usize) < 64 {
                    (*tag & mask(*total_width)) << tag_lo
                } else {
                    0
                };
                for (slot, lo) in parts.iter() {
                    if (*lo as usize) < 64 {
                        bits |= cn[slot] << *lo;
                    }
                }
                let out = bits & mask(*total_width);
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::EncodeCtorWide { dst, .. } => {
            // Delegates to `TypeLayoutIndex::encode_ctor`; folding it
            // would duplicate layout logic, so just invalidate.
            invalidate_dst(Dst::Wide(*dst), cn, cw);
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Splice { total_width, parts, dst } => {
            let all_const = parts.iter().all(|(src, _, _)| match src {
                Src::Narrow(s) => cn.contains_key(s),
                Src::Wide(s) => cw.contains_key(s),
            });
            if all_const {
                match *dst {
                    Dst::Narrow(d) => {
                        let mut bits = 0u64;
                        for (src, lo, _) in parts.iter() {
                            if (*lo as usize) < 64 {
                                let v = match src {
                                    Src::Narrow(s) => cn[s],
                                    Src::Wide(s) => cw[s].to_u64(),
                                };
                                bits |= v << *lo;
                            }
                        }
                        let out = bits & mask(*total_width);
                        code[ip] = Ins::Const { bits: out, dst: d };
                        cn.insert(d, out);
                    }
                    Dst::Wide(d) => {
                        let mut out = BitPat::zeros(*total_width);
                        for (src, lo, w) in parts.iter() {
                            let pat = match src {
                                Src::Narrow(s) => BitPat::from_u64(*w, cn[s]),
                                Src::Wide(s) => cw[s].clone(),
                            };
                            for i in 0..pat.width() as usize {
                                if pat.get_bit(i) && *lo as usize + i < *total_width as usize {
                                    out.set_bit(*lo as usize + i, true);
                                }
                            }
                        }
                        code[ip] = Ins::WideConst { bits: out.clone(), dst: d };
                        cw.insert(d, out);
                    }
                }
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Index { subj, index, dst } => {
            if let Some(a) = n_const!(*subj) {
                let out = if (*index as usize) < 64 { (a >> index) & 1 } else { 0 };
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::IndexWide { subj, index, dst } => {
            if let Some(pat) = w_const!(*subj) {
                let out = pat.get_bit(*index as usize) as u64;
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::IndexDyn { subj, index, dst } => {
            if let (Some(a), Some(idx)) = (n_const!(*subj), n_const!(*index)) {
                let out = if idx < 64 { (a >> idx) & 1 } else { 0 };
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::IndexDynWide { subj, index, dst } => {
            if let (Some(pat), Some(idx)) = (w_const!(*subj), n_const!(*index)) {
                let out = pat.get_bit(idx as usize) as u64;
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::IndexRange { subj, lo, hi, dst } => {
            if let Some(a) = n_const!(*subj) {
                let out = if (*lo as usize) < 64 {
                    (a >> lo) & mask(hi.saturating_sub(*lo))
                } else {
                    0
                };
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::IndexRangeWide { subj, lo, hi, dst } => {
            if let Some(pat) = w_const!(*subj) {
                let sliced = pat.slice(*lo, *hi);
                write_const_dst(*dst, &sliced, cn, cw);
                code[ip] = dst_ins(*dst, &sliced);
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Zext { subj, out_width, dst } => {
            if let Some(a) = n_const!(*subj) {
                let bits = a & mask(*out_width);
                match *dst {
                    Dst::Narrow(d) => {
                        code[ip] = Ins::Const { bits, dst: d };
                        cn.insert(d, bits);
                    }
                    // A wide destination needs the slot's static
                    // width to build the pattern; leave it to run.
                    Dst::Wide(_) => {
                        invalidate_dst(*dst, cn, cw);
                    }
                }
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::ZextWide { subj, out_width, dst } => {
            if let Some(pat) = w_const!(*subj) {
                let out = pat.zext_to(*out_width);
                write_const_dst(*dst, &out, cn, cw);
                code[ip] = dst_ins(*dst, &out);
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Sext { subj, subj_width, out_width, dst } => {
            if let Some(a) = n_const!(*subj) {
                if *out_width <= 64 {
                    // Transcribes `eval_sext`'s Small path.
                    let msb = if *subj_width == 0 {
                        false
                    } else {
                        (a >> (subj_width - 1)) & 1 != 0
                    };
                    let out = if *out_width >= *subj_width {
                        if msb {
                            a | (mask(*out_width) & !mask(*subj_width))
                        } else {
                            a & mask(*out_width)
                        }
                    } else {
                        a & mask(*out_width)
                    };
                    match *dst {
                        Dst::Narrow(d) => {
                            let bits = out & mask(*out_width);
                            code[ip] = Ins::Const { bits, dst: d };
                            cn.insert(d, bits);
                        }
                        // A wide destination needs the slot's static
                        // width to build the pattern; leave it to run.
                        Dst::Wide(_) => {
                            invalidate_dst(*dst, cn, cw);
                        }
                    }
                } else {
                    // Big path: `bits.sext_to(w)`.
                    let pat = BitPat::from_u64(*subj_width, a).sext_to(*out_width);
                    write_const_dst(*dst, &pat, cn, cw);
                    code[ip] = dst_ins(*dst, &pat);
                }
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::SextWide { subj, out_width, dst } => {
            if let Some(pat) = w_const!(*subj) {
                let out = pat.sext_to(*out_width);
                write_const_dst(*dst, &out, cn, cw);
                code[ip] = dst_ins(*dst, &out);
            } else {
                invalidate_dst(*dst, cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Move { src, dst } => {
            if let Some(bits) = n_const!(*src) {
                code[ip] = Ins::Const { bits, dst: *dst };
                cn.insert(*dst, bits);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::MoveWide { src, dst } => {
            if let Some(pat) = w_const!(*src) {
                code[ip] = Ins::WideConst { bits: pat.clone(), dst: *dst };
                cw.insert(*dst, pat);
            } else {
                invalidate_dst(Dst::Wide(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Any { subj, dst } => {
            if let Some(a) = n_const!(*subj) {
                let out = (a != 0) as u64;
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::All { subj, width, dst } => {
            if let Some(a) = n_const!(*subj) {
                let out = (*width == 64 || a == mask(*width)) as u64;
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::AnyWide { subj, dst } => {
            if let Some(pat) = w_const!(*subj) {
                let out = (!pat.is_zero()) as u64;
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::AllWide { subj, dst } => {
            if let Some(pat) = w_const!(*subj) {
                let out = pat.is_all_ones() as u64;
                code[ip] = Ins::Const { bits: out, dst: *dst };
                cn.insert(*dst, out);
            } else {
                invalidate_dst(Dst::Narrow(*dst), cn, cw);
            }
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Ins::Trap | Ins::Ret { .. } | Ins::RetWide { .. } => {
            *arrival = Arrival::Walk;
            ip + 1
        }
    }
}

/// Build the `Const`/`WideConst` instruction writing `pat` into `dst`,
/// transcribing `write_pat` (narrow results are truncated to their
/// low 64 bits).
fn dst_ins(dst: Dst, pat: &BitPat) -> Ins {
    match dst {
        Dst::Narrow(d) => Ins::Const { bits: pat.to_u64(), dst: d },
        Dst::Wide(d) => Ins::WideConst { bits: pat.clone(), dst: d },
    }
}

/// Record a folded `BitPat` result into the constant environment
/// (mirroring `write_pat`).
fn write_const_dst(dst: Dst, pat: &BitPat, cn: &mut HashMap<SlotN, u64>, cw: &mut HashMap<SlotW, BitPat>) {
    match dst {
        Dst::Narrow(d) => {
            cn.insert(d, pat.to_u64());
        }
        Dst::Wide(d) => {
            cw.insert(d, pat.clone());
        }
    }
}

fn invalidate_dst(dst: Dst, cn: &mut HashMap<SlotN, u64>, cw: &mut HashMap<SlotW, BitPat>) {
    match dst {
        Dst::Narrow(d) => {
            cn.remove(&d);
        }
        Dst::Wide(d) => {
            cw.remove(&d);
        }
    }
}

/// A constant-condition branch: rewrite to `Jmp` and pick the path.
///
/// `take` is `None` when the condition is not constant (no rewrite,
/// fall through with a valid map).  When `Some(true)` (branch taken),
/// try to skip the dead region between here and the target so the
/// constant environment survives into the target; otherwise walk it
/// with a cleared map.  When `Some(false)` (fall-through), the
/// branch's edge into the target disappears, so rewrite to a no-op
/// `Jmp` (the executor's falsy path is `ip + 1`, which is identical).
fn handle_branch(
    code: &mut Vec<Ins>,
    ip: usize,
    take: Option<bool>,
    target: usize,
    jump_preds: &[u32],
    arrival: &mut Arrival,
) -> usize {
    match take {
        None => {
            // Fall-through is a real edge; the map stays valid (the
            // branch wrote nothing).
            *arrival = Arrival::Fresh;
            ip + 1
        }
        Some(true) => {
            code[ip] = Ins::Jmp { to: target as Ip };
            if region_skippable(ip, target, jump_preds) {
                // The dead region has no other entries and the target
                // has no other in-edge, so the environment carries.
                *arrival = Arrival::Edge;
                target
            } else {
                // Walk the region (it may contain live jump targets);
                // the join at the target clears the environment.
                *arrival = Arrival::Walk;
                ip + 1
            }
        }
        Some(false) => {
            code[ip] = Ins::Jmp { to: (ip + 1) as Ip };
            *arrival = Arrival::Edge;
            ip + 1
        }
    }
}

/// Can the scan jump straight from `ip` to `target`, treating
/// `(ip, target)` as dead?  True when the region has no jump-target
/// entries and the target's only in-edge is this branch.
fn region_skippable(ip: usize, target: usize, jump_preds: &[u32]) -> bool {
    if target <= ip + 1 || target >= jump_preds.len() {
        return false;
    }
    (ip + 1..target).all(|j| jump_preds[j] == 0) && jump_preds[target] == 1
}

// ---------------------------------------------------------------------
// Pass 2a: common-subexpression elimination
// ---------------------------------------------------------------------

/// Canonical key for a pure compute instruction: (discriminant,
/// operands, static parameters).  `None` for non-computes, Loads and
/// anything with side effects.
fn cse_key(ins: &Ins) -> Option<(u8, Vec<u64>)> {
    let disc = std::mem::discriminant(ins);
    let key = match ins {
        Ins::Bin { op, width, lhs, rhs, .. } => {
            (*op as u8, vec![*width as u64, *lhs as u64, *rhs as u64])
        }
        Ins::BinWide { op, lhs, rhs, .. } => {
            (*op as u8, vec![*lhs as u64, *rhs as u64])
        }
        Ins::Un { op, width, subj, .. } => (*op as u8, vec![*width as u64, *subj as u64]),
        Ins::UnWide { op, subj, .. } => (*op as u8, vec![*subj as u64]),
        Ins::Extract { subj, lo, width, .. } => {
            (2, vec![*subj as u64, *lo as u64, *width as u64])
        }
        Ins::ExtractWide { subj, lo, width, .. } => {
            (2, vec![*subj as u64, *lo as u64, *width as u64])
        }
        Ins::Index { subj, index, .. } => (3, vec![*subj as u64, *index as u64]),
        Ins::IndexWide { subj, index, .. } => (3, vec![*subj as u64, *index as u64]),
        Ins::IndexRange { subj, lo, hi, .. } => (4, vec![*subj as u64, *lo as u64, *hi as u64]),
        Ins::IndexRangeWide { subj, lo, hi, .. } => (4, vec![*subj as u64, *lo as u64, *hi as u64]),
        Ins::Zext { subj, out_width, .. } => (5, vec![*subj as u64, *out_width as u64]),
        Ins::ZextWide { subj, out_width, .. } => (5, vec![*subj as u64, *out_width as u64]),
        Ins::Sext { subj, subj_width, out_width, .. } => {
            (6, vec![*subj as u64, *subj_width as u64, *out_width as u64])
        }
        Ins::SextWide { subj, out_width, .. } => (6, vec![*subj as u64, *out_width as u64]),
        Ins::Any { subj, .. } => (7, vec![*subj as u64]),
        Ins::All { subj, width, .. } => (7, vec![*subj as u64, *width as u64]),
        Ins::AnyWide { subj, .. } => (7, vec![*subj as u64]),
        Ins::AllWide { subj, .. } => (7, vec![*subj as u64]),
        Ins::Move { src, .. } | Ins::MoveWide { src, .. } => (8, vec![*src as u64]),
        _ => return None,
    };
    let _ = disc;
    // Fold the discriminant into the first key byte so narrow and
    // wide variants never collide.
    let disc_byte = match ins {
        Ins::Bin { .. } | Ins::Un { .. } | Ins::Extract { .. } | Ins::Index { .. }
        | Ins::IndexRange { .. } | Ins::Zext { .. } | Ins::Sext { .. } | Ins::Any { .. }
        | Ins::All { .. } | Ins::Move { .. } => 0u8,
        Ins::BinWide { .. } | Ins::UnWide { .. } | Ins::ExtractWide { .. } | Ins::IndexWide { .. }
        | Ins::IndexRangeWide { .. } | Ins::ZextWide { .. } | Ins::SextWide { .. }
        | Ins::AnyWide { .. } | Ins::AllWide { .. } | Ins::MoveWide { .. } => 1u8,
        _ => 0u8,
    };
    Some((disc_byte, key.1))
}

/// Eliminate redundant computes within a driver: a compute whose key
/// was already computed into a live slot becomes a `Move` from that
/// slot.  Only safe within straight-line regions without intervening
/// rewrites of the operand slots; the table is invalidated at control
/// flow joins and after any instruction that could alias the table.
fn cse(code: &mut Vec<Ins>) -> bool {
    let len = code.len();
    if len == 0 {
        return false;
    }
    let mut changed = false;
    // key -> (dst slot, result width) of the first computation.
    let mut table: HashMap<(u8, Vec<u64>), (Dst, u16)> = HashMap::new();
    for ip in 0..len {
        let key = cse_key(&code[ip]);
        match key {
            Some(key) => {
                let dst = dst_of(&code[ip]);
                if let Some((prev_dst, prev_width)) = table.get(&key) {
                    // Only rewrite when the two results land in the
                    // same slot class (narrow vs wide), so the `Move`
                    // shape is legal.
                    if std::mem::discriminant(&dst) == std::mem::discriminant(prev_dst) {
                        let new_ins = match (dst, *prev_dst) {
                            (Dst::Narrow(d), Dst::Narrow(s)) => Ins::Move { src: s, dst: d },
                            (Dst::Wide(d), Dst::Wide(s)) => Ins::MoveWide { src: s, dst: d },
                            _ => unreachable!("discriminant check passed"),
                        };
                        code[ip] = new_ins;
                        changed = true;
                        continue;
                    }
                    let _ = prev_width;
                }
                table.insert(key, (dst, result_width(&code[ip])));
            }
            None => {
                // Invalidate the table when a slot this instruction
                // writes could be a table entry's source.
                if let Some(dst) = def_dst(&code[ip]) {
                    table.retain(|_, (prev_dst, _)| *prev_dst != dst);
                }
                // Non-computes (branches, loads, control) kill all
                // entries: their position in the block is not
                // comparable.
                if !is_pure_compute(&code[ip]) {
                    table.clear();
                }
            }
        }
    }
    changed
}

fn is_pure_compute(ins: &Ins) -> bool {
    matches!(
        ins,
        Ins::Bin { .. }
            | Ins::BinWide { .. }
            | Ins::Un { .. }
            | Ins::UnWide { .. }
            | Ins::Extract { .. }
            | Ins::ExtractWide { .. }
            | Ins::Index { .. }
            | Ins::IndexWide { .. }
            | Ins::IndexDyn { .. }
            | Ins::IndexDynWide { .. }
            | Ins::IndexRange { .. }
            | Ins::IndexRangeWide { .. }
            | Ins::Zext { .. }
            | Ins::ZextWide { .. }
            | Ins::Sext { .. }
            | Ins::SextWide { .. }
            | Ins::Move { .. }
            | Ins::MoveWide { .. }
            | Ins::Any { .. }
            | Ins::All { .. }
            | Ins::AnyWide { .. }
            | Ins::AllWide { .. }
            | Ins::EncodeCtor { .. }
            | Ins::EncodeCtorWide { .. }
            | Ins::Splice { .. }
    )
}

fn dst_of(ins: &Ins) -> Dst {
    match ins {
        // Narrow destinations.
        Ins::Bin { dst, .. }
        | Ins::Un { dst, .. }
        | Ins::Extract { dst, .. }
        | Ins::Index { dst, .. }
        | Ins::IndexRange { dst, .. }
        | Ins::Move { dst, .. }
        | Ins::Any { dst, .. }
        | Ins::All { dst, .. }
        | Ins::IndexWide { dst, .. }
        | Ins::IndexDyn { dst, .. }
        | Ins::IndexDynWide { dst, .. }
        | Ins::AnyWide { dst, .. }
        | Ins::AllWide { dst, .. }
        | Ins::EncodeCtor { dst, .. }
        | Ins::Const { dst, .. }
        | Ins::LoadUnresolved { dst } => Dst::Narrow(*dst),
        // Wide destinations.
        Ins::WideConst { dst, .. }
        | Ins::EncodeCtorWide { dst, .. }
        | Ins::MoveWide { dst, .. } => Dst::Wide(*dst),
        // `Dst` destinations.
        Ins::Load { dst, .. }
        | Ins::LoadWord { dst, .. }
        | Ins::LoadWordDyn { dst, .. }
        | Ins::Zero { dst, .. }
        | Ins::ExtractWide { dst, .. }
        | Ins::BinWide { dst, .. }
        | Ins::UnWide { dst, .. }
        | Ins::Splice { dst, .. }
        | Ins::IndexRangeWide { dst, .. }
        | Ins::Zext { dst, .. }
        | Ins::ZextWide { dst, .. }
        | Ins::Sext { dst, .. }
        | Ins::SextWide { dst, .. } => *dst,
        _ => Dst::Narrow(0),
    }
}

/// The result's bit width for a compute instruction (used to keep CSE
/// entries honest about the destination slot's static width;
/// CSE only compares same-shaped instructions, so the slot class
/// implies the width).
fn result_width(ins: &Ins) -> u16 {
    match ins {
        Ins::Bin { width, .. } | Ins::Un { width, .. } | Ins::Extract { width, .. } => *width,
        Ins::Zext { out_width, .. } | Ins::Sext { out_width, .. } => *out_width,
        _ => 0,
    }
}

// ---------------------------------------------------------------------
// Pass 2b: slot fusion
// ---------------------------------------------------------------------

/// Fuse single-use producer->consumer chains: when a compute's result
/// slot is read exactly once, rewrite the compute to write the
/// consumed operand's slot and repoint the reader, eliminating the
/// intermediate slot (and any `Move` it fed).
///
/// Guarded so the rewrite stays inside one straight-line region: the
/// reader must be found before any jump target or terminator, and the
/// fused operand slot must not be rewritten in between.
fn slot_fusion(code: &mut Vec<Ins>) -> bool {
    let len = code.len();
    if len == 0 {
        return false;
    }
    let mut changed = false;
    // Each successful fusion retires a slot or removes a Move, so
    // this terminates; the cap is belt and braces.
    for _ in 0..=len {
        let fused_any = run_one_fusion_round(code, len);
        if !fused_any {
            break;
        }
        changed = true;
    }
    changed
}

fn run_one_fusion_round(code: &mut Vec<Ins>, len: usize) -> bool {
    {
        let (n_narrow, n_wide) = frame_extent(code);
        let total = n_narrow as usize + n_wide as usize;
        // Combined use counts: narrow slots [0, n_narrow), wide slots
        // [n_narrow, n_narrow + n_wide).
        let mut counts = vec![0usize; total];
        // Writer counts: a convergence slot is written once per arm,
        // so a copy-prop through one arm's `Move` is only safe when
        // the destination has exactly one writer.
        let mut writers = vec![0usize; total];
        let slot_of = |slot: Dst| slot_index(slot, n_narrow);
        for ins in code.iter() {
            for_each_slot(ins, &mut |slot| counts[slot_of(slot)] += 1);
            if let Some(dst) = def_dst(ins) {
                writers[slot_of(dst)] += 1;
            }
        }
        let mut fused = false;
        for ip in 0..code.len() {
            let ins = code[ip].clone();
            // Copy propagation: a `Move` whose destination is read
            // exactly once by a later instruction in the same
            // straight-line chain is spliced out (the reader reads the
            // Move's source directly), removing the Move.
            let Some(dst) = def_dst(&ins) else { continue };
            let is_move = matches!(ins, Ins::Move { .. } | Ins::MoveWide { .. });
            if !is_move && counts[slot_of(dst)] != 1 {
                continue;
            }
            if is_move {
                // Copy propagation: the Move's source feeds the
                // single reader of the Move's destination.
                let (src_slot, dst_slot) = match &ins {
                    Ins::Move { src, dst } => (Dst::Narrow(*src), Dst::Narrow(*dst)),
                    Ins::MoveWide { src, dst } => (Dst::Wide(*src), Dst::Wide(*dst)),
                    _ => unreachable!(),
                };
                if counts[slot_of(dst_slot)] != 1 || writers[slot_of(dst_slot)] != 1 {
                    continue;
                }
                // Nothing may write the Move's source between the Move
                // and the reader (the copy's value must survive).
                let mut blocked = false;
                let mut reader = None;
                let mut j = ip + 1;
                while j < len {
                    if is_control(&code[j]) {
                        break;
                    }
                    if def_dst(&code[j]).map_or(false, |d| d == src_slot) {
                        blocked = true;
                        break;
                    }
                    if def_dst(&code[j]).map_or(false, |d| d == dst_slot) {
                        break;
                    }
                    let mut reads = false;
                    for_each_slot(&code[j], &mut |slot| {
                        if slot == dst_slot {
                            reads = true;
                        }
                    });
                    if reads {
                        reader = Some(j);
                        break;
                    }
                    j += 1;
                }
                if blocked {
                    continue;
                }
                let Some(reader) = reader else { continue };
                rewrite_uses(&mut code[reader], dst_slot, src_slot);
                let mut dead_mask = vec![false; code.len()];
                dead_mask[ip] = true;
                rebuild(code, &dead_mask);
                fused = true;
                break;
            }
            // Slot retargeting: a compute whose destination is read
            // exactly once and that has a single-use operand of the
            // same slot class writes the operand's slot instead, so
            // the destination slot retires.
            let mut fuse_src: Option<Dst> = None;
            for_each_slot(&ins, &mut |slot| {
                if fuse_src.is_some() {
                    return;
                }
                if std::mem::discriminant(&slot) != std::mem::discriminant(&dst) {
                    return;
                }
                if slot != dst && counts[slot_of(slot)] == 1 {
                    fuse_src = Some(slot);
                }
            });
            let Some(fuse_src) = fuse_src else { continue };
            let mut blocked = false;
            let mut reader = None;
            let mut j = ip + 1;
            while j < len {
                if is_control(&code[j]) {
                    break;
                }
                let d = def_dst(&code[j]);
                if d.map_or(false, |x| x == fuse_src) {
                    blocked = true;
                    break;
                }
                if d.map_or(false, |x| x == dst) {
                    break;
                }
                let mut reads = false;
                for_each_slot(&code[j], &mut |slot| {
                    if slot == dst {
                        reads = true;
                    }
                });
                if reads {
                    reader = Some(j);
                    break;
                }
                j += 1;
            }
            if blocked {
                continue;
            }
            let Some(reader) = reader else { continue };
            // The op must not read its own destination (the fused
            // write would clobber the read).
            let mut reads_dst = false;
            for_each_slot(&code[ip], &mut |slot| {
                if slot == dst {
                    reads_dst = true;
                }
            });
            if reads_dst {
                continue;
            }
            rewrite_dst(&mut code[ip], fuse_src);
            rewrite_uses(&mut code[reader], dst, fuse_src);
            fused = true;
            break;
        }
        if !fused {
            return false;
        }
        return true;
    }
}

/// Rewrite an instruction's destination slot.
fn rewrite_dst(ins: &mut Ins, new_dst: Dst) {
    match ins {
        // `Dst` destinations: assign directly.
        Ins::Load { dst, .. }
        | Ins::LoadWord { dst, .. }
        | Ins::LoadWordDyn { dst, .. }
        | Ins::Zero { dst, .. }
        | Ins::ExtractWide { dst, .. }
        | Ins::BinWide { dst, .. }
        | Ins::UnWide { dst, .. }
        | Ins::Splice { dst, .. }
        | Ins::IndexRangeWide { dst, .. }
        | Ins::Zext { dst, .. }
        | Ins::ZextWide { dst, .. }
        | Ins::Sext { dst, .. }
        | Ins::SextWide { dst, .. } => *dst = new_dst,
        // Narrow destinations.
        Ins::Bin { dst, .. }
        | Ins::Un { dst, .. }
        | Ins::Extract { dst, .. }
        | Ins::Index { dst, .. }
        | Ins::IndexRange { dst, .. }
        | Ins::Move { dst, .. }
        | Ins::Any { dst, .. }
        | Ins::All { dst, .. }
        | Ins::IndexWide { dst, .. }
        | Ins::IndexDyn { dst, .. }
        | Ins::IndexDynWide { dst, .. }
        | Ins::AnyWide { dst, .. }
        | Ins::AllWide { dst, .. }
        | Ins::EncodeCtor { dst, .. }
        | Ins::Const { dst, .. }
        | Ins::LoadUnresolved { dst } => *dst = unwrap_narrow(new_dst),
        // Wide destinations.
        Ins::WideConst { dst, .. }
        | Ins::EncodeCtorWide { dst, .. }
        | Ins::MoveWide { dst, .. } => *dst = unwrap_wide(new_dst),
        _ => {}
    }
}

fn unwrap_narrow(dst: Dst) -> SlotN {
    match dst {
        Dst::Narrow(s) => s,
        Dst::Wide(_) => unreachable!("narrow slot expected"),
    }
}

fn unwrap_wide(dst: Dst) -> SlotW {
    match dst {
        Dst::Wide(s) => s,
        Dst::Narrow(_) => unreachable!("wide slot expected"),
    }
}

/// Redirect every read of slot `from` in this instruction to `to`
/// (same slot class only).
fn rewrite_uses(ins: &mut Ins, from: Dst, to: Dst) {
    let sub_n = |slot: &mut SlotN| {
        if from == Dst::Narrow(*slot) {
            if let Dst::Narrow(t) = to {
                *slot = t;
            }
        }
    };
    let sub_w = |slot: &mut SlotW| {
        if from == Dst::Wide(*slot) {
            if let Dst::Wide(t) = to {
                *slot = t;
            }
        }
    };
    match ins {
        Ins::Jmp { .. } => {}
        Ins::BrTruthy { cond, .. } => sub_n(cond),
        Ins::TestTag { subj, .. } => sub_n(subj),
        Ins::TestTagWide { subj, .. } => sub_w(subj),
        Ins::Extract { subj, .. } => sub_n(subj),
        Ins::ExtractWide { subj, .. } => sub_w(subj),
        Ins::TestBits { subj, .. } | Ins::TestBit { subj, .. } => sub_n(subj),
        Ins::Bin { lhs, rhs, .. } => {
            sub_n(lhs);
            sub_n(rhs);
        }
        Ins::BinWide { lhs, rhs, .. } => {
            sub_w(lhs);
            sub_w(rhs);
        }
        Ins::Un { subj, .. } => sub_n(subj),
        Ins::UnWide { subj, .. } => sub_w(subj),
        Ins::EncodeCtor { parts, .. } => {
            for (slot, _) in parts.iter_mut() {
                sub_n(slot);
            }
        }
        Ins::EncodeCtorWide { args, .. } => {
            for (src, _) in args.iter_mut() {
                match src {
                    Src::Narrow(s) => sub_n(s),
                    Src::Wide(s) => sub_w(s),
                }
            }
        }
        Ins::Splice { parts, .. } => {
            for (src, _, _) in parts.iter_mut() {
                match src {
                    Src::Narrow(s) => sub_n(s),
                    Src::Wide(s) => sub_w(s),
                }
            }
        }
        Ins::Index { subj, .. } | Ins::IndexRange { subj, .. } => sub_n(subj),
        Ins::IndexWide { subj, .. } => sub_w(subj),
        Ins::IndexDyn { subj, index, .. } => {
            sub_n(subj);
            sub_n(index);
        }
        Ins::IndexDynWide { subj, index, .. } => {
            sub_w(subj);
            sub_n(index);
        }
        Ins::IndexRangeWide { subj, .. } => sub_w(subj),
        Ins::Zext { subj, .. } | Ins::Sext { subj, .. } => sub_n(subj),
        Ins::ZextWide { subj, .. } | Ins::SextWide { subj, .. } => sub_w(subj),
        Ins::Move { src, .. } => sub_n(src),
        Ins::MoveWide { src, .. } => sub_w(src),
        Ins::Any { subj, .. } | Ins::All { subj, .. } => sub_n(subj),
        Ins::AnyWide { subj, .. } | Ins::AllWide { subj, .. } => sub_w(subj),
        Ins::Load { .. }
        | Ins::LoadWord { .. }
        | Ins::LoadUnresolved { .. }
        | Ins::Const { .. }
        | Ins::WideConst { .. }
        | Ins::Zero { .. } => {}
        Ins::LoadWordDyn { index, .. } => sub_n(index),
        Ins::Trap => {}
        Ins::Ret { src } => sub_n(src),
        Ins::RetWide { src } => sub_w(src),
    }
}

// ---------------------------------------------------------------------
// Pass 2c: clean (no-op jumps, UCE, dead writes)
// ---------------------------------------------------------------------

fn clean(code: &mut Vec<Ins>) -> bool {
    let mut changed = false;
    loop {
        let mut any = false;
        if remove_noop_jumps(code) {
            any = true;
        }
        if remove_unreachable(code) {
            any = true;
        }
        if remove_dead_writes(code) {
            any = true;
        }
        if !any {
            break;
        }
        changed = true;
    }
    changed
}

fn remove_noop_jumps(code: &mut Vec<Ins>) -> bool {
    let dead: Vec<bool> = code
        .iter()
        .enumerate()
        .map(|(ip, ins)| matches!(ins, Ins::Jmp { to } if *to as usize == ip + 1))
        .collect();
    if dead.iter().any(|&d| d) {
        rebuild(code, &dead);
        true
    } else {
        false
    }
}

fn successors(ins: &Ins, ip: usize, len: usize) -> Vec<usize> {
    match ins {
        Ins::Jmp { to } => vec![*to as usize],
        Ins::BrTruthy { to, .. } => {
            let mut s = vec![ip + 1];
            if (*to as usize) < len {
                s.push(*to as usize);
            }
            s
        }
        Ins::TestTag { next_if_match, .. }
        | Ins::TestTagWide { next_if_match, .. }
        | Ins::TestBits { next_if_match, .. }
        | Ins::TestBit { next_if_match, .. } => {
            let mut s = vec![ip + 1];
            if (*next_if_match as usize) < len {
                s.push(*next_if_match as usize);
            }
            s
        }
        Ins::Trap | Ins::Ret { .. } | Ins::RetWide { .. } => Vec::new(),
        _ => {
            if ip + 1 < len {
                vec![ip + 1]
            } else {
                Vec::new()
            }
        }
    }
}

fn remove_unreachable(code: &mut Vec<Ins>) -> bool {
    let len = code.len();
    if len == 0 {
        return false;
    }
    let mut reachable = vec![false; len];
    let mut stack = vec![0usize];
    reachable[0] = true;
    while let Some(ip) = stack.pop() {
        for succ in successors(&code[ip], ip, len) {
            if succ < len && !reachable[succ] {
                reachable[succ] = true;
                stack.push(succ);
            }
        }
    }
    let dead: Vec<bool> = reachable.iter().map(|&r| !r).collect();
    if dead.iter().any(|&d| d) {
        rebuild(code, &dead);
        true
    } else {
        false
    }
}

/// Backward-liveness dead-instruction elimination: drop slots written
/// but never read (dead `Load`s and pure ops).
fn remove_dead_writes(code: &mut Vec<Ins>) -> bool {
    let len = code.len();
    if len == 0 {
        return false;
    }
    let (n_narrow, n_wide) = frame_extent(code);
    let total = n_narrow as usize + n_wide as usize;
    if total == 0 {
        return false;
    }
    // live_in[ip] computed backward to a fixpoint.
    let mut live_in: Vec<Vec<bool>> = vec![vec![false; total]; len];
    let mut changed = true;
    let mut rounds = 0;
    while changed {
        changed = false;
        rounds += 1;
        if rounds > len + 1 {
            break;
        }
        for ip in (0..len).rev() {
            let ins = &code[ip];
            let mut live_out = vec![false; total];
            for succ in successors(ins, ip, len) {
                if succ < len {
                    for i in 0..total {
                        live_out[i] = live_out[i] || live_in[succ][i];
                    }
                }
            }
            // live_in = uses ∪ (live_out - defs): the def is removed
            // first so an instruction that reads and writes the same
            // slot keeps its own use (the read sees the old value).
            let mut li = live_out.clone();
            if let Some(dst) = def_dst(ins) {
                li[slot_index(dst, n_narrow)] = false;
            }
            for_each_slot(ins, &mut |slot| {
                li[slot_index(slot, n_narrow)] = true;
            });
            if li != live_in[ip] {
                live_in[ip] = li;
                changed = true;
            }
        }
    }
    let dead: Vec<bool> = code
        .iter()
        .enumerate()
        .map(|(ip, ins)| {
            if matches!(ins, Ins::Jmp { .. } | Ins::BrTruthy { .. } | Ins::TestTag { .. }
                | Ins::TestTagWide { .. } | Ins::TestBits { .. } | Ins::TestBit { .. }
                | Ins::Trap | Ins::Ret { .. } | Ins::RetWide { .. })
            {
                return false;
            }
            let live_out = successors(ins, ip, len)
                .into_iter()
                .fold(vec![false; total], |mut acc, succ| {
                    if succ < len {
                        for i in 0..total {
                            acc[i] = acc[i] || live_in[succ][i];
                        }
                    }
                    acc
                });
            match def_dst(ins) {
                Some(dst) => !is_live(dst, &live_out, n_narrow),
                None => false,
            }
        })
        .collect();
    if dead.iter().any(|&d| d) {
        rebuild(code, &dead);
        true
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::elaboration::SignalId;
    use crate::common::BinOp;
    use crate::sim::value::Value;
    use crate::sim::vm::exec::VmState;
    use crate::sim::vm::frame::Vm;
    use crate::sim::vm::isa::{CodeRange, FrameLayout, Program};

    /// A table-backed `VmState` (same shape as the executor's unit
    /// tests): narrow bits per signal index.
    struct TestState {
        narrow: Vec<u64>,
    }

    impl VmState for TestState {
        fn narrow_bits(&self, signal: SignalId) -> u64 {
            self.narrow.get(signal.index()).copied().unwrap_or(0)
        }
        fn wide_bits(&self, _signal: SignalId) -> BitPat {
            BitPat::zeros(0)
        }
        fn memory_word_narrow(&self, _signal: SignalId, _index: usize) -> Option<u64> {
            None
        }
        fn memory_word_wide(&self, _signal: SignalId, _index: usize) -> Option<BitPat> {
            None
        }
    }

    /// Optimize `ins` and run it as a single driver returning from
    /// `ret_slot`, with `n_narrow` frame slots and `state` as engine
    /// state.  The driver's value is a `ret_width`-bit `Word`.
    fn optimize_and_run(
        mut ins: Vec<Ins>,
        ret_slot: SlotN,
        n_narrow: u16,
        state: TestState,
    ) -> (Value, Vec<Ins>) {
        optimize_driver(&mut ins);
        let (layouts, typ_id) = {
            let mut interner = crate::sim::layout::TypeInterner::new();
            let id = interner.intern(&crate::types::Type::Word(8));
            (crate::sim::layout::TypeLayoutIndex::from_parts(Default::default(), interner), id)
        };
        let program = Program {
            code: ins.clone().into_boxed_slice(),
            entry: vec![Some(CodeRange { start: 0, len: ins.len() as Ip })],
            frames: vec![FrameLayout {
                n_narrow,
                n_wide: 0,
                wide_widths: Box::new([]),
            }],
            driver_typs: vec![Some(typ_id)],
            driver_widths: vec![8],
            layouts,
            const_values: vec![None],
            schedule: None,
        };
        let mut vm = Vm::new(&program);
        let value = vm.run_out(&program, &state, SignalId::from_index(0)).into_value();
        let _ = ret_slot;
        (value, ins)
    }

    /// A driver that is entirely constant folds to `Const; Ret` (the
    /// constant-unit skip's canonical shape), and the executed value
    /// is the folded constant.
    #[test]
    fn all_constant_driver_folds_to_const_ret() {
        // (3 & 5) + 1 = 2, computed through a chain of folds.
        let ins = vec![
            Ins::Const { bits: 3, dst: 0 },
            Ins::Const { bits: 5, dst: 1 },
            Ins::Bin { op: BinOp::And, width: 8, lhs: 0, rhs: 1, dst: 2 },
            Ins::Const { bits: 1, dst: 3 },
            Ins::Bin { op: BinOp::Add, width: 8, lhs: 2, rhs: 3, dst: 4 },
            Ins::Ret { src: 4 },
        ];
        let state = TestState { narrow: vec![0; 8] };
        let (value, out) = optimize_and_run(ins, 4, 8, state);
        assert_eq!(value.to_u64(), 2);
        assert_eq!(value.width(), 8);
        assert_eq!(out.len(), 2, "folded to Const + Ret: {out:?}");
        assert!(matches!(out[0], Ins::Const { bits: 2, .. }), "{out:?}");
        assert!(matches!(out[1], Ins::Ret { .. }), "{out:?}");
    }

    /// A driver with a constant-condition branch: the dispatch folds
    /// to an unconditional jump, the untaken arm (and the `Trap`
    /// inside it) is removed, and the driver returns the taken arm's
    /// value.  The interpreter also never executed the dead branch's
    /// `Trap`, so removing it preserves the lazy-failure timing.
    #[test]
    fn dead_branch_with_trap_is_removed() {
        // when (2 == 2) { 7 } else { <trap> }
        let ins = vec![
            Ins::Const { bits: 1, dst: 0 },
            Ins::BrTruthy { cond: 0, kind: Truthiness::Bit, to: 3 },
            Ins::Jmp { to: 5 },
            Ins::Const { bits: 7, dst: 1 },
            Ins::Jmp { to: 7 },
            Ins::Trap,
            Ins::Jmp { to: 7 },
            Ins::Move { src: 1, dst: 2 },
            Ins::Ret { src: 2 },
        ];
        let state = TestState { narrow: vec![0; 8] };
        let (value, out) = optimize_and_run(ins, 2, 8, state);
        assert_eq!(value.to_u64(), 7);
        // The dead arm (with its Trap) is gone.
        assert!(
            out.iter().all(|ins| !matches!(ins, Ins::Trap)),
            "the unreachable Trap was removed: {out:?}"
        );
    }

    /// A `Trap` behind a data-dependent condition is reachable in
    /// principle and must survive every pass (the lazy failure stays
    /// lazy).
    #[test]
    fn reachable_trap_is_kept() {
        let ins = vec![
            Ins::Load { signal: SignalId::from_index(1), dst: Dst::Narrow(0) },
            Ins::BrTruthy { cond: 0, kind: Truthiness::Bit, to: 3 },
            Ins::Jmp { to: 5 },
            Ins::Const { bits: 7, dst: 1 },
            Ins::Jmp { to: 7 },
            Ins::Trap,
            Ins::Jmp { to: 7 },
            Ins::Move { src: 1, dst: 2 },
            Ins::Ret { src: 2 },
        ];
        // Signal 1 truthy: the taken arm runs, the Trap stays lazy.
        let state = TestState { narrow: vec![0, 1, 0, 0, 0, 0, 0, 0] };
        let (_, out) = optimize_and_run(ins, 2, 8, state);
        assert!(
            out.iter().any(|ins| matches!(ins, Ins::Trap)),
            "a reachable Trap must not be deleted: {out:?}"
        );
    }

    /// A constant-condition branch whose dead arm computes into the
    /// same slot class must not disturb the live arm's slots: the
    /// fold removes the arm before any slot fusion can retarget
    /// across it.
    #[test]
    fn constant_condition_selects_the_arm() {
        // when true { 7 } else { <data> }
        let ins = vec![
            Ins::Const { bits: 1, dst: 0 },
            Ins::BrTruthy { cond: 0, kind: Truthiness::Bit, to: 3 },
            Ins::Jmp { to: 6 },
            Ins::Const { bits: 7, dst: 2 },
            Ins::Jmp { to: 7 },
            Ins::Load { signal: SignalId::from_index(2), dst: Dst::Narrow(1) },
            Ins::Jmp { to: 7 },
            Ins::Move { src: 2, dst: 3 },
            Ins::Ret { src: 3 },
        ];
        let state = TestState { narrow: vec![0, 0, 0b1101, 0, 0, 0, 0, 0] };
        let (value, out) = optimize_and_run(ins, 3, 8, state);
        assert_eq!(value.to_u64(), 7);
        // The dead arm's Load is gone.
        assert!(
            out.iter().all(|ins| !matches!(ins, Ins::Load { .. })),
            "the dead arm's Load was removed: {out:?}"
        );
    }
}
