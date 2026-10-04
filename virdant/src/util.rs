//! Standalone utility functions: constructing a `Db` from directories or
//! files (loading the builtin library automatically), word/nat literal
//! parsing, bit-width helpers (`min_word_width`, `log2`), and `check_db`
//! to run all checks and partition diagnostics by severity.

use std::sync::Arc;

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

    let names: Vec<BString> = files.iter().map(|(name, _)| name.clone()).collect();
    db.set_packages(PackageTable::new(names));
    let packages = db.get_packages();
    for (name, text) in files {
        let package = packages.id(name.as_bstr()).expect("package missing from table");
        db.set_source(package, Source::new(package, text));
    }
    db
}

#[cfg(not(target_arch = "wasm32"))]
fn load_vir_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Vec<(BString, BString)> {
    let builtin = Source::load_file(crate::LIB_DIR.join("builtin.vir"));
    let mut files = vec![builtin];
    let source_dir: std::path::PathBuf = source_dir.into();
    for filepath in std::fs::read_dir(&source_dir).expect(&format!("Could not open directory: {source_dir:?}")) {
        let filepath = match filepath {
            Ok(filepath) => filepath.path(),
            Err(_) => continue,
        };
        match filepath.extension() {
            Some(ext) if ext.to_string_lossy() == "vir" => (),
            _ => continue,
        }
        files.push(Source::load_file(filepath));
    }
    files
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Db {
    db_from_name_text_pairs(load_vir_dir(source_dir))
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir_with_lib<P, Q>(source_dir: P, lib_dir: Q) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf> {
    let lib_dir = lib_dir.into();
    let lib_dir = std::fs::canonicalize(&lib_dir).expect(&format!("Could not find {lib_dir:?}"));
    let builtin = Source::load_file(lib_dir.join("builtin.vir"));
    let mut files = vec![builtin];
    let source_dir: std::path::PathBuf = source_dir.into();
    for filepath in std::fs::read_dir(&source_dir).expect(&format!("Could not open directory: {source_dir:?}")) {
        let filepath = match filepath {
            Ok(filepath) => filepath.path(),
            Err(_) => continue,
        };
        match filepath.extension() {
            Some(ext) if ext.to_string_lossy() == "vir" => (),
            _ => continue,
        }
        files.push(Source::load_file(filepath));
    }
    db_from_name_text_pairs(files)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_file<P: Into<std::path::PathBuf>>(source_file: P) -> Db {
    db_from_files_with_lib(vec![source_file], crate::LIB_DIR.as_path())
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_file_with_lib<P, Q>(source_file: P, lib_dir: Q) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf> {
    db_from_files_with_lib(vec![source_file], lib_dir)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_files<P: Into<std::path::PathBuf>>(source_files: Vec<P>) -> Db {
    db_from_files_with_lib(source_files, crate::LIB_DIR.as_path())
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_files_with_lib<P, Q>(source_files: Vec<P>, lib_dir: Q) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf> {
    let lib_dir = lib_dir.into();
    let lib_dir = std::fs::canonicalize(&lib_dir).expect(&format!("Could not find {lib_dir:?}"));

    let builtin = Source::load_file(lib_dir.join("builtin.vir"));
    let mut files = vec![builtin];

    for source_file in source_files {
        files.push(Source::load_file(source_file.into()));
    }

    db_from_name_text_pairs(files)
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
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

pub fn parse_nat_literal(literal: &str) -> WordValue {
    let literal = literal.replace('_', "");
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
