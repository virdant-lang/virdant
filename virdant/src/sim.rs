mod bits;
mod builtins;
mod circuit;
mod eval;
mod expr;
mod layout;
mod payload;
mod scheduler;
mod sim;
mod state;
mod vcd;

/// The `virt::Virt` board, built on top of this engine.
/// Requires the `vir-bin` feature for the `nix` dependency.
#[cfg(feature = "vir-bin")]
pub mod virt;

pub use sim::Sim;
pub use eval::Value;
