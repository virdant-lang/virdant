//! Standalone utility functions: the package-name rule shared by the
//! compiler and the tools, word/nat literal parsing, bit-width helpers
//! (`min_word_width`, `log2`), and `check_db` to run all checks and
//! partition diagnostics by severity.
//!
//! Filesystem loading of `.vir` files (directory scans, `Db`
//! construction from files) lives in the tools' crate
//! (`vircommon::load`), since the core is also built for wasm.

use std::sync::Arc;

use bstr::BStr;
use bstr::ByteSlice as _;

use crate::common::{Width, WordValue};
use crate::db::Db;
use crate::diagnostics::{Diagnostic, DiagnosticLevel};

/// Whether `name` is a valid package name: it must match the lexer's
/// identifier rule `[_a-zA-Z][_a-zA-Z0-9]*` (see `syntax/token.rs`),
/// because `import` accepts only an `Ident`.  Package names come from
/// `.vir` file stems, so a file whose stem is not a valid identifier
/// yields a package that no other file can ever import.
pub fn is_valid_package_name(name: &BStr) -> bool {
    let bytes = name.as_bytes();
    match bytes.split_first() {
        None => false,
        Some((first, rest)) => {
            let first_ok = first.is_ascii_alphabetic() || *first == b'_';
            let rest_ok = rest
                .iter()
                .all(|b| b.is_ascii_alphanumeric() || *b == b'_');
            first_ok && rest_ok
        }
    }
}

pub fn min_word_width(value: WordValue) -> Width {
    if value == 0 {
        0
    } else {
        u64::BITS as Width - u64::leading_zeros(value) as Width
    }
}

/// Returns `Some(k)` if `n` is a power of two (`n = 2^k`), else `None`.
pub fn log2(n: Width) -> Option<Width> {
    if n == 0 || (n & (n - 1)) != 0 {
        None
    } else {
        Some(n.trailing_zeros() as Width)
    }
}

pub fn parse_word_literal(literal: &str) -> (WordValue, Option<Width>) {
    if let Some((value, width)) = literal.split_once('w') {
        // TODO: Handle overflow instead of panicking.
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

pub fn parse_nat_literal(literal: &str) -> WordValue {
    let literal = literal.replace('_', "");
    // NOTE: The lexer (logos) can emit a bare `0x`/`0b` prefix as a `Nat`
    // token while the text is mid-edit. It has no digits, hence no value,
    // so it is treated as zero rather than panicking.
    if literal == "0x" || literal == "0b" {
        return 0;
    }
    // TODO: Handle overflow (PosOverflow) instead of panicking.
    if let Some(hex) = literal.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap()
    } else if let Some(bin) = literal.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap()
    } else {
        literal.parse().unwrap()
    }
}

pub fn check_db(db: &Db) -> Result<Arc<Vec<Diagnostic>>, Arc<Vec<Diagnostic>>> {
    let diagnostics = db.check();
    if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
        Err(diagnostics)
    } else {
        Ok(diagnostics)
    }
}
