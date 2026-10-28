//! The Virdant test runner library (`virtue`).
//!
//! A pure orchestrator: it links nothing from the compiler.  Every test
//! is a subprocess of `target/release/vir` (plus iverilog / verilator
//! for the sim suite), run with the same commands and exit-code
//! contracts the per-directory Makefiles used before the runner
//! replaced them.
//!
//! Public modules:
//! - [`discovery`]: finding tests and the `test.toml` format
//! - [`exec`]: running one test, evidence logs
//! - [`output`]: screen rendering and the end summary
//! - [`filecheck`]: the CHECK-directive matcher (also exposed via
//!   `virtue --filecheck` and the `filecheck` compatibility binary)

pub mod discovery;
pub mod exec;
pub mod filecheck;
pub mod output;
