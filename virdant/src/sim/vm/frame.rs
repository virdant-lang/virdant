//! Pooled per-driver frames: the replacement for the interpreter's
//! per-eval `Context`.  One frame per driven component, allocated
//! once in `Vm::new` and reused for every evaluation, the
//! steady-state hot path never allocates a frame.
//!
//! Slots are only ever addressed within their own component's frame
//! (the executor indexes `frames[comp.index()]`), so "a slot index is
//! valid only inside its driver" is enforced by construction.
//!
//! Wide slots carry the slot's static width (`FrameLayout::
//! wide_widths`) so the executor can build zero-extended / truncated
//! patterns without re-deriving it; narrow slots are raw `u64`s whose
//! widths live only in the compiler (every writing instruction masks
//! to the slot's width).

use crate::sim::bits::BitPat;
use crate::sim::vm::isa::FrameLayout;

/// One component's scratch frame: raw bits for slots of ≤64-bit
/// static types, `BitPat`s for wider ones.
pub(super) struct Frame {
    pub(super) narrow: Vec<u64>,
    pub(super) wide: Vec<BitPat>,
}

impl Frame {
    pub(super) fn from_layout(layout: &FrameLayout) -> Frame {
        Frame {
            narrow: vec![0; layout.n_narrow as usize],
            wide: layout.wide_widths.iter().map(|w| BitPat::zeros(*w)).collect(),
        }
    }
}

/// The frame pool: one frame per elaborated component, densely
/// indexed by `SignalId::index()`.  Frames for undriven components
/// are empty.
pub(in crate::sim) struct Vm {
    pub(super) frames: Vec<Frame>,
}

impl Vm {
    pub(in crate::sim) fn new(program: &crate::sim::vm::isa::Program) -> Vm {
        Vm { frames: program.frames.iter().map(Frame::from_layout).collect() }
    }
}
