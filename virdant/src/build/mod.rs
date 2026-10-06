//! The build module: PCF constraint emission and FPGA toolchain
//! runners/flashers, keyed by the platform's `@fpga` family.

#[cfg(not(target_arch = "wasm32"))]
pub mod constraints;
#[cfg(not(target_arch = "wasm32"))]
pub mod toolchain;

#[cfg(not(target_arch = "wasm32"))]
pub use constraints::*;
#[cfg(not(target_arch = "wasm32"))]
pub use toolchain::*;
