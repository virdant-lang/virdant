//! Scripting support for Virdant simulation.
//!
//! Currently supports Rhai scripting. Lua support planned for the future.

pub mod rhai;

pub use rhai::run_script_file;
