//! The discrete-event simulation engine
//!
//! Values are bit patterns tagged with their type (see `bits` and
//! `layout`); there are no X or Z values — uninitialized cells take a
//! value from the [`InitStrategy`] (random by default).

mod bits;
mod builtins;
mod expr;
mod payload;
mod layout;
mod eval;

mod circuit;
mod sim;
mod vcd;

mod scheduler;
mod state;

/// The `virt::Virt` board, built on top of this engine.  Requires the
/// `vir-bin` feature (the terminal/`nix` dependency).
#[cfg(feature = "vir-bin")]
pub mod virt;

use scheduler::Scheduler;
use state::State;

pub use bits::BitPat;
pub use layout::TypeLayoutIndex;

pub use sim::SimError;
pub use sim::{Sim, SimLock, Clock, InitStrategy};
pub use eval::Value;
pub use scheduler::Callback;
