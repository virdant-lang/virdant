//! The compiled-sim VM: ISA (`isa`), per-driver compiler (`compile`),
//! pooled frames (`frame`), and executor (`exec`).
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

pub(in crate::sim) use compile::{compile_initial_value, compile_program};
pub(in crate::sim) use frame::Vm;
pub(in crate::sim) use exec::{RunOut, VmState};
pub(in crate::sim) use isa::Program;
