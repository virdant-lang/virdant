//! Standalone utility functions: the package-name rule shared by the
//! compiler and the tools, word/nat literal parsing, bit-width helpers
//! (`min_word_width`, `log2`), `check_db` to run all checks and
//! partition diagnostics by severity, and (for tests and in-crate
//! tooling such as the simulator's `#[cfg(test)]` fixtures) a
//! self-contained `Db` builder from a source directory plus an
//! explicit library directory.
//!
//! The tools' full loading machinery lives in the tools' crate
//! (`vircommon::load`), since the core is also built for wasm; the
//! `db_from_dir_with_lib` here is the minimal in-crate seam the sim
//! tests need.

use std::sync::Arc;

use bstr::BStr;
use bstr::BString;
use bstr::ByteSlice as _;

use crate::common::{Width, WordValue};
use crate::db::Db;
use crate::diagnostics::{Diagnostic, DiagnosticLevel};

#[cfg(not(target_arch = "wasm32"))]
use crate::common::source::Source;
#[cfg(not(target_arch = "wasm32"))]
use crate::package::PackageTable;

#[cfg(not(target_arch = "wasm32"))]
fn db_from_name_text_pairs(files: Vec<(BString, BString)>) -> Db {
    let mut db = Db::new();

    let deduped: indexmap::IndexMap<BString, BString> = files.into_iter().collect();
    db.set_packages(PackageTable::new(deduped.keys().cloned().collect()));
    let packages = db.get_packages();
    for (name, text) in deduped {
        let package = packages.id(name.as_bstr()).expect("package missing from table");
        db.set_source(package, Source::new(package, text));
    }
    db
}

#[cfg(not(target_arch = "wasm32"))]
fn load_lib_dir<P: Into<std::path::PathBuf>>(lib_dir: P) -> Vec<(BString, BString)> {
    let lib_dir = std::fs::canonicalize(lib_dir.into())
        .expect("Could not find lib directory");
    let mut filepaths: Vec<std::path::PathBuf> = std::fs::read_dir(&lib_dir)
        .expect(&format!("Could not open directory: {lib_dir:?}"))
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|filepath| match filepath.extension() {
            Some(ext) => ext.to_string_lossy() == "vir",
            None => false,
        })
        .collect();
    // Deterministic package ordering regardless of read_dir order.
    filepaths.sort();
    filepaths.into_iter().map(Source::load_file).collect()
}

#[cfg(not(target_arch = "wasm32"))]
fn load_source_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Vec<(BString, BString)> {
    let source_dir: std::path::PathBuf = source_dir.into();
    let mut filepaths: Vec<std::path::PathBuf> = std::fs::read_dir(&source_dir)
        .expect(&format!("Could not open directory: {source_dir:?}"))
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|filepath| match filepath.extension() {
            Some(ext) => ext.to_string_lossy() == "vir",
            None => false,
        })
        .collect();
    filepaths.sort();
    filepaths.into_iter().map(Source::load_file).collect()
}

/// Build a self-contained `Db` from a source directory plus an explicit
/// library directory (the in-crate seam the sim's `#[cfg(test)]`
/// fixtures need; tools use `vircommon::load` instead).  Every `.vir`
/// file in either directory is loaded, package name = file stem.
#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir_with_lib<P, Q>(source_dir: P, lib_dir: Q) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf> {
    let mut files = load_lib_dir(lib_dir);
    files.extend(load_source_dir(source_dir));
    db_from_name_text_pairs(files)
}

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

/// Decodes the C-style escapes of a string literal's raw text (the
/// text between the quotes, as written in the source): the
/// single-character escapes `\a \b \e \f \n \r \t \v` and
/// `\\ \' \" \?`, octal escapes `\ooo` of one to three octal digits
/// (so `\0` is the NUL byte), and hex escapes `\xNN` of exactly two
/// hex digits.  An octal escape above `\377` wraps modulo 256, as in
/// C.  The lexer's `Str` rule (syntax/token.rs) accepts exactly these
/// escape forms; anything else reaching this function is copied
/// through verbatim.
pub fn decode_string_escapes(raw: &[u8]) -> BString {
    let mut decoded = BString::new(vec![]);
    let mut index = 0;
    while index < raw.len() {
        if raw[index] != b'\\' || index + 1 >= raw.len() {
            decoded.push(raw[index]);
            index += 1;
            continue;
        }
        let escape = raw[index + 1];
        match escape {
            b'a' => { decoded.push(0x07); index += 2; }
            b'b' => { decoded.push(0x08); index += 2; }
            b'e' => { decoded.push(0x1b); index += 2; }
            b'f' => { decoded.push(0x0c); index += 2; }
            b'n' => { decoded.push(b'\n'); index += 2; }
            b'r' => { decoded.push(b'\r'); index += 2; }
            b't' => { decoded.push(b'\t'); index += 2; }
            b'v' => { decoded.push(0x0b); index += 2; }
            b'\\' | b'\'' | b'"' | b'?' => {
                decoded.push(escape);
                index += 2;
            }
            b'x' => {
                // `\xNN`: exactly two hex digits (the lexer's `Str`
                // rule requires them); anything else is copied
                // through verbatim.
                let digits = raw.get(index + 2..index + 4);
                let Some(digits) = digits else {
                    decoded.push(raw[index]);
                    index += 1;
                    continue;
                };
                match std::str::from_utf8(digits).ok().and_then(|digits| u8::from_str_radix(digits, 16).ok()) {
                    Some(value) => {
                        decoded.push(value);
                        index += 4;
                    }
                    None => {
                        decoded.push(raw[index]);
                        index += 1;
                    }
                }
            }
            b'0'..=b'7' => {
                // `\ooo`: one to three octal digits; values above
                // `\377` wrap modulo 256, as in C.
                let mut value: u32 = 0;
                let mut digits = 0;
                while digits < 3
                    && index + 1 + digits < raw.len()
                    && (b'0'..=b'7').contains(&raw[index + 1 + digits])
                {
                    value = value * 8 + (raw[index + 1 + digits] - b'0') as u32;
                    digits += 1;
                }
                decoded.push((value & 0xff) as u8);
                index += 1 + digits;
            }
            _ => {
                // Not an escape form the lexer accepts; copy through.
                decoded.push(raw[index]);
                index += 1;
            }
        }
    }
    decoded
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
