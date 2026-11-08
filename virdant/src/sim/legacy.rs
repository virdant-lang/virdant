//! The legacy simulator engine: master's tree-walking interpreter
//! (`Expr::eval`), split out of the original `Sim` so the public `Sim`
//! facade (`sim::sim`) can host the engine-agnostic machinery
//! (scheduler, watcher table, bucketed dispatch, free-running clocks)
//! on top of it.
//!
//! `engine.rs` holds `LegacySim` — the engine body proper; `eval.rs`
//! the `Expr` interpreter and its `Context`; `state.rs` the live
//! values, pending register values, dirty worklist, and the
//! watched-signal changed-set bookkeeping the facade's bucketed
//! dispatch reads.

pub(crate) mod engine;
pub(crate) mod eval;
pub(crate) mod state;
