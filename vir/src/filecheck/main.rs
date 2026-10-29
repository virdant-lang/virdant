//! Compatibility shim: `vir-filecheck <check-file>` behaves exactly like
//! `vir-virtue --filecheck <check-file>`.  Kept for direct invocations
//! of the matcher (and `make install`).

#[path = "../virtue/filecheck.rs"]
mod filecheck;

fn main() {
    filecheck::cli_main(std::env::args().nth(1));
}
