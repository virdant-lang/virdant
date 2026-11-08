//! Human-readable disassembly of a compiled `Program`.
//!
//! One section per driven component (its own little program), branch
//! targets rendered as labels, signal references rendered by their
//! elaborated path. This is the format `vir run --bytecode` prints and
//! the format `vm/INSTRUCTIONS.md` (repo root) documents.

use super::isa::{Dst, Ins, Program, Truthiness};
use crate::analysis::elaboration::SignalId;

fn slot(dst: &Dst) -> String {
    match dst {
        Dst::Narrow(n) => format!("s{n}"),
        Dst::Wide(n) => format!("w{n}"),
    }
}

#[allow(dead_code)] // dead on this ISA revision (the branch's disassembler carried it)
fn src_str(src: &super::isa::Src) -> String {
    match src {
        super::isa::Src::Narrow(n) => format!("s{n}"),
        super::isa::Src::Wide(n) => format!("w{n}"),
    }
}

/// Disassemble every driven component's program. `names` maps a
/// `SignalId::index()` to its elaborated path (`top.count`, ...).
pub(in crate::sim) fn disassemble(program: &Program, names: &[bstr::BString]) -> String {
    let mut out = String::new();
    let nm = |s: &SignalId| String::from_utf8_lossy(&names[s.index()]).to_string();
    for (idx, comp) in program.entry.iter().enumerate() {
        let Some(range) = comp else { continue };
        let name = String::from_utf8_lossy(&names[idx]).to_string();
        out.push_str(&format!("--- {name} ({} ins) ---\n", range.len));
        let lo = range.start as usize;
        let hi = (range.start + range.len) as usize;
        let mut targets: Vec<u32> = Vec::new();
        for ins in &program.code[lo..hi] {
            match ins {
                Ins::Jmp { to } | Ins::BrTruthy { to, .. } => targets.push(*to),
                Ins::TestTag { next_if_match, .. }
                | Ins::TestTagWide { next_if_match, .. }
                | Ins::TestBits { next_if_match, .. }
                | Ins::TestBit { next_if_match, .. } => targets.push(*next_if_match),
                _ => {}
            }
        }
        targets.sort_unstable();
        targets.dedup();
        for i in lo..hi {
            if targets.contains(&(i as u32)) {
                out.push_str(&format!("L{i}:\n"));
            }
            out.push_str(&format!("  {}\n", print_ins(&program.code[i], &nm)));
        }
    }
    out
}

fn print_ins(ins: &Ins, nm: &dyn Fn(&SignalId) -> String) -> String {
    let lbl = |to: &u32| format!("L{to}");
    match ins {
        Ins::Load { signal, dst } => format!("load {}, {}", slot(dst), nm(signal)),
        Ins::LoadUnresolved { dst } => format!("loadunresolved s{dst}"),
        Ins::Const { bits, dst } => format!("const s{dst}, {bits:#x}"),
        Ins::WideConst { bits, dst } => format!("const.w w{dst}, {bits:?}"),
        Ins::Zero { dst, width } => format!("zero {}, width={width}", slot(dst)),
        Ins::Jmp { to } => format!("jmp {}", lbl(to)),
        Ins::BrTruthy { cond, kind, to } => format!(
            "brtruthy{} s{cond}, {}",
            match kind {
                Truthiness::Bit => ".bit",
                Truthiness::NonZero => ".nz",
            },
            lbl(to)
        ),
        Ins::TestTag { subj, tag_lo, tag_mask, tag, next_if_match } => format!(
            "testtag s{subj}, lo={tag_lo}, mask={tag_mask:#x}, tag={tag}, {}",
            lbl(next_if_match)
        ),
        Ins::TestTagWide { subj, tag, next_if_match } => {
            format!("testtag.w w{subj}, tag={tag:?}, {}", lbl(next_if_match))
        }
        Ins::Extract { subj, lo, width, dst } => {
            format!("extract s{dst}, s{subj}, lo={lo}, width={width}")
        }
        Ins::ExtractWide { subj, lo, width, dst } => format!(
            "extract.w {}, w{subj}, lo={lo}, width={width}",
            slot(dst)
        ),
        Ins::TestBits { subj, value, next_if_match } => {
            format!("testbits s{subj}, {value:#x}, {}", lbl(next_if_match))
        }
        Ins::TestBit { subj, bit, next_if_match } => {
            format!("testbit s{subj}, {}, {}", bit, lbl(next_if_match))
        }
        Ins::Bin { op, width, lhs, rhs, dst } => {
            format!("bin.{op:?} s{dst}, s{lhs}, s{rhs} ; w={width}")
        }
        Ins::BinWide { op, lhs, rhs, dst } => {
            format!("bin.{op:?}.w {}, w{lhs}, w{rhs}", slot(dst))
        }
        Ins::Un { op, width, subj, dst } => format!("un.{op:?} s{dst}, s{subj} ; w={width}"),
        Ins::UnWide { op, subj, dst } => format!("un.{op:?}.w {}, w{subj}", slot(dst)),
        Ins::EncodeCtor { total_width, tag, tag_lo, parts, dst } => format!(
            "encode s{dst}, tag={tag}, lo={tag_lo}, parts={parts:?} ; total={total_width}"
        ),
        Ins::EncodeCtorWide { typ, symbol, total_width, args, dst } => format!(
            "encode.w w{dst}, typ={typ:?}, sym={symbol:?}, args={args:?} ; total={total_width}"
        ),
        Ins::Splice { total_width, parts, dst } => format!(
            "splice {}, parts={parts:?} ; total={total_width}",
            slot(dst)
        ),
        Ins::Index { subj, index, dst } => format!("index s{dst}, s{subj}, {index}"),
        Ins::IndexWide { subj, index, dst } => format!("index.w s{dst}, w{subj}, {index}"),
        Ins::IndexDyn { subj, index, dst } => format!("indexdyn s{dst}, s{subj}, s{index}"),
        Ins::IndexDynWide { subj, index, dst } => {
            format!("indexdyn.w s{dst}, w{subj}, s{index}")
        }
        Ins::IndexRange { subj, lo, hi, dst } => {
            format!("indexrange s{dst}, s{subj}, lo={lo}, hi={hi}")
        }
        Ins::IndexRangeWide { subj, lo, hi, dst } => format!(
            "indexrange.w {}, w{subj}, lo={lo}, hi={hi}",
            slot(dst)
        ),
        Ins::Zext { subj, out_width, dst } => format!("zext {}, s{subj}, {out_width}", slot(dst)),
        Ins::ZextWide { subj, out_width, dst } => {
            format!("zext.w {}, w{subj}, {out_width}", slot(dst))
        }
        Ins::Sext { subj, subj_width, out_width, dst } => format!(
            "sext {}, s{subj}, w={subj_width}, {out_width}",
            slot(dst)
        ),
        Ins::SextWide { subj, out_width, dst } => {
            format!("sext.w {}, w{subj}, {out_width}", slot(dst))
        }
        Ins::Move { src, dst } => format!("move s{dst}, s{src}"),
        Ins::MoveWide { src, dst } => format!("move.w w{dst}, w{src}"),
        Ins::Any { subj, dst } => format!("any s{dst}, s{subj}"),
        Ins::All { subj, width, dst } => format!("all s{dst}, s{subj} ; w={width}"),
        Ins::AnyWide { subj, dst } => format!("any.w s{dst}, w{subj}"),
        Ins::AllWide { subj, dst } => format!("all.w s{dst}, w{subj}"),
        Ins::Trap => "trap".to_string(),
        Ins::Ret { src } => format!("ret s{src}"),
        Ins::RetWide { src } => format!("ret.w w{src}"),
    }
}
