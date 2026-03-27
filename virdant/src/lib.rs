pub mod vir;
pub mod common;
pub mod source;
pub mod fqn;
pub mod syntax;
pub mod db;
pub mod analysis;
pub mod queries;
pub mod diagnostics;
pub mod virir;
pub mod conversion;
pub mod verilog;
pub mod transpile;
pub mod types;

pub use vir::Vir;

use std::sync::LazyLock;

pub const EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| {
    std::fs::canonicalize(std::path::PathBuf::from("../examples")).unwrap()
});

pub const LIB_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| {
    let exe = std::env::current_exe().unwrap();
    let root = exe.parent().unwrap().parent().unwrap();
    let lib = root.join("lib");
    std::fs::canonicalize(lib).unwrap()
});
