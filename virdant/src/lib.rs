pub mod common;
pub mod fqn;
pub mod syntax;
pub mod db;
pub mod analysis;
pub mod queries;
pub mod diagnostics;
pub mod verilog;
pub mod types;
pub mod docs;
pub mod util;
pub mod sim;
#[cfg(not(feature = "wasm"))]
pub mod script;

#[cfg(not(target_arch = "wasm32"))]
use std::sync::LazyLock;

#[cfg(not(target_arch = "wasm32"))]
pub const EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| {
    std::fs::canonicalize(std::path::PathBuf::from("../examples")).unwrap()
});

#[cfg(not(target_arch = "wasm32"))]
pub const LIB_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| {
    let exe = std::env::current_exe().unwrap();
    let root = exe.parent().unwrap().parent().unwrap();
    let lib = root.join("lib");
    std::fs::canonicalize(&lib).expect(&format!("Could not find {lib:?}"))
});
