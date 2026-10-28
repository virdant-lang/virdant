//! Compatibility shim: `filecheck <check-file>` behaves exactly like
//! `virtue --filecheck <check-file>`.  Kept for direct invocations
//! of the matcher (and `make install`).

fn main() {
    virtue::filecheck::cli_main(std::env::args().nth(1));
}
