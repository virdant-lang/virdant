//! Where the standard library lives, per platform.
//!
//! Two variants with exactly the same API:
//!
//! * [`native`] — the host: the installed `lib/` directory, found
//!   relative to the running executable (see the `Makefile`
//!   `install` target).
//! * [`wasm32`] — WASI targets: the same files, embedded at compile
//!   time (the host layout has no meaning under wasmtime).
//!
//! This module re-exports the active variant, so callers never gate
//! on `target_arch` themselves.

#[cfg(not(target_arch = "wasm32"))]
pub mod native;

#[cfg(target_arch = "wasm32")]
pub mod wasm32;

#[cfg(not(target_arch = "wasm32"))]
pub use native::*;

#[cfg(target_arch = "wasm32")]
pub use wasm32::*;
