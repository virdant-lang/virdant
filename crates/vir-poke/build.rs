//! Build script: generate the poke scripting grammar
//! (`src/grammar.lalrpop`) into OUT_DIR (included by `lib.rs` via
//! `lalrpop_mod!(grammar, "/grammar.rs")`).  The vanilla lalrpop
//! crate suffices here — no need for the modified version the
//! `virdant` crate uses.

fn main() {
    lalrpop::process_root().unwrap();
    println!("cargo:rerun-if-changed=src/grammar.lalrpop");
}
