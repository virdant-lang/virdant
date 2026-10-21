//! Hub module for the type system, re-exporting `Type`, `Typing`,
//! `ExprRoot`, and `TypingContext`, and declaring submodules for typing
//! contexts, type representations, typing inference, typedefs,
//! signatures, and match coverage.

pub mod context;
pub mod render;
pub mod typ;
pub mod typing;
pub mod typedef;
pub mod signature;
pub mod substitute;
pub mod width;
pub mod match_coverage;

pub use context::TypingContext;
pub use typ::{Kind, ParamId, Type};
pub use substitute::substitute;
pub use typing::{ExprRoot, Typing};
