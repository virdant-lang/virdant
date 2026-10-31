//! The compiled-sim VM: ISA (`isa`), per-driver compiler (`compile`),
//! pooled frames (`frame`), executor (`exec`), and the static
//! combinational schedule (`schedule`).
//!
//! Everything here is internal to `sim`: the public surface (`Sim`,
//! `Value`, ...) is unchanged.  `Sim` owns a `Program` (compiled
//! drivers) and a `Vm` (frame pool); its per-driver execution seam
//! (`Sim::eval_driver`) runs compiled bytecode, the only evaluation
//! engine (see `compile.rs`).

mod compile;
mod exec;
mod frame;
mod isa;
mod optimize;
mod schedule;
// The Cranelift JIT v2 backend (`jit` cargo feature): batch-level
// native compilation of qualifying deep-cycle replay phases — one
// native function per phase, evaluating all of the phase's drivers in
// one call with direct state addressing.  All of its code compiles
// out when the feature is off (see `jit.rs`).
#[cfg(feature = "jit")]
mod jit;
// The JIT v2 batch-phase surface `sim.rs` drives: the compiled-phase
// container and its constructor (called at schedule activation).
// Everything else stays inside the `vm` tree.
#[cfg(feature = "jit")]
pub(in crate::sim) use jit::{build_phases, PhaseJit};

pub(in crate::sim) use compile::{compile_initial_value, compile_program};
pub(in crate::sim) use frame::Vm;
pub(in crate::sim) use exec::{RunOut, VmState};
pub(in crate::sim) use isa::Program;
pub(in crate::sim) use schedule::{CycleSchedule, CycleState, Phase, ScheduleCache};
