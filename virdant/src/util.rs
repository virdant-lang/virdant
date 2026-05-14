use std::sync::Arc;

use crate::common::source::Source;
use crate::db::Db;
use crate::diagnostics::{Diagnostic, DiagnosticLevel};
use crate::LIB_DIR;

pub fn db_from_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Db {
    let mut db = Db::new();
    db.set_packages(vec![]);
    let builtin_source = Source::load_file(LIB_DIR.join("builtin.vir"));
    let mut sources = vec![builtin_source.clone()];
    db.set_source(builtin_source.package(), builtin_source);
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
        let source = Source::load_file(filepath);
        db.set_source(source.package(), source.clone());
        sources.push(source);
    }
    db.set_packages(sources.iter().map(|source| source.package()).collect());
    db
}

pub fn db_from_dir_with_lib<P, Q>(source_dir: P, lib_dir: Q) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf> {
    let mut db = Db::new();
    db.set_packages(vec![]);

    let lib_dir = lib_dir.into();
    let lib_dir = std::fs::canonicalize(&lib_dir).expect(&format!("Could not find {lib_dir:?}"));
    let builtin_source = Source::load_file(lib_dir.join("builtin.vir"));
    let mut sources = vec![builtin_source.clone()];
    db.set_source(builtin_source.package(), builtin_source);
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
        let source = Source::load_file(filepath);
        db.set_source(source.package(), source.clone());
        sources.push(source);
    }
    db.set_packages(sources.iter().map(|source| source.package()).collect());
    db
}

pub fn db_from_file<P: Into<std::path::PathBuf>>(source_file: P) -> Db {
    db_from_file_with_lib(source_file, LIB_DIR.as_path())
}

pub fn db_from_file_with_lib<P, Q>(source_file: P, lib_dir: Q) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf> {
    let mut db = Db::new();
    db.set_packages(vec![]);

    let lib_dir = lib_dir.into();
    let lib_dir = std::fs::canonicalize(&lib_dir).expect(&format!("Could not find {lib_dir:?}"));

    let builtin_source = Source::load_file(lib_dir.join("builtin.vir"));
    let file_source = Source::load_file(source_file.into());

    let builtin_package = builtin_source.package();
    let file_package = file_source.package();

    db.set_source(builtin_package.clone(), builtin_source);
    db.set_source(file_package.clone(), file_source);
    db.set_packages(vec![builtin_package, file_package]);

    db
}

pub fn check_db(db: &Db) -> Result<Arc<Vec<Diagnostic>>, Arc<Vec<Diagnostic>>> {
    let diagnostics = db.check();
    if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
        Err(diagnostics)
    } else {
        Ok(diagnostics)
    }
}
