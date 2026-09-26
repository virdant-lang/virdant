//! Hub module for the type system, re-exporting `Type`, `Typing`,
//! `ExprRoot`, and `TypingContext`, and declaring submodules for typing
//! contexts, type representations, typing inference, typedefs,
//! signatures, and match coverage.

pub mod context;
pub mod typ;
pub mod typing;
pub mod typedef;
pub mod signature;
pub mod match_coverage;

pub use context::TypingContext;
pub use typ::Type;
pub use typing::{ExprRoot, Typing};
