//! Filesystem loading of `.vir` sources: scanning project source
//! directories, and building analysis `Db`s from a directory or an
//! explicit file list (always loading the standard library), with
//! deterministic package ordering.
//!
//! This lives in the tools' crate rather than the compiler core: the
//! core is also built for wasm, and how to *report* a file whose name
//! is not a valid package identifier is a tooling decision anyway
//! (fail loudly in the CLI, skip with a warning in the LSP).  The core
//! provides only the pure predicate, `virdant::util::is_valid_package_name`.

use std::path::PathBuf;

use bstr::BString;
use bstr::ByteSlice as _;

use virdant::common::source::Source;
use virdant::db::Db;
use virdant::package::PackageTable;
use virdant::util::is_valid_package_name;

/// The package name a `.vir` file is loaded as: its file stem.
fn package_name_of(filepath: &std::path::Path) -> BString {
    BString::from(filepath.file_stem().unwrap_or_default().as_encoded_bytes())
}

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

fn load_lib_dir<P: Into<std::path::PathBuf>>(lib_dir: P) -> Vec<(BString, BString)> {
    let lib_dir = std::fs::canonicalize(lib_dir.into())
        .expect("Could not find lib directory");
    let mut filepaths: Vec<PathBuf> = std::fs::read_dir(&lib_dir)
        .expect(&format!("Could not open directory: {lib_dir:?}"))
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|filepath| match filepath.extension() {
            Some(ext) => ext.to_string_lossy() == "vir",
            None => false,
        })
        .collect();
    // Deterministic package ordering regardless of read_dir order.
    filepaths.sort();
    // The standard library ships with the compiler; a bad name there is
    // a bug in Virdant itself, not a user error, so panic.
    for filepath in &filepaths {
        assert!(
            is_valid_package_name(package_name_of(filepath).as_bstr()),
            "Invalid package name in standard library: {}",
            filepath.display(),
        );
    }
    filepaths.into_iter().map(Source::load_file).collect()
}

/// The result of scanning a project source directory for `.vir` files.
/// `invalid_names` holds the paths whose stems are not valid package
/// names; this module does not say *how* to report them — tools decide
/// whether to fail (CLI) or skip with a warning (LSP).
pub struct SourceDirScan {
    pub valid: Vec<PathBuf>,
    pub invalid_names: Vec<PathBuf>,
}

/// Scan a project source directory for `.vir` files, sorted, without
/// loading them.  The `Err` variant covers only real I/O errors
/// (unreadable directory); a file with an invalid package name is not
/// an `Err`, it lands in `SourceDirScan::invalid_names`.
pub fn scan_source_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> std::io::Result<SourceDirScan> {
    let source_dir: PathBuf = source_dir.into();
    let mut filepaths: Vec<PathBuf> = std::fs::read_dir(&source_dir)
        .map_err(|e| std::io::Error::new(e.kind(), format!("Could not open directory: {source_dir:?}: {e}")))?
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|filepath| match filepath.extension() {
            Some(ext) => ext.to_string_lossy() == "vir",
            None => false,
        })
        .collect();
    filepaths.sort();
    let (valid, invalid_names) = filepaths.into_iter().partition(|filepath| {
        is_valid_package_name(package_name_of(filepath).as_bstr())
    });
    Ok(SourceDirScan { valid, invalid_names })
}

/// Build the whole-project `Db` (standard library plus source
/// directory) from a [`scan_source_dir`] result.  Loading is not
/// scoping: only `import` puts a package's items in scope.
pub fn db_from_source_dir_scan(scan: SourceDirScan) -> Db {
    let mut files = load_lib_dir(virdant::LIB_DIR.as_path());
    files.extend(scan.valid.into_iter().map(Source::load_file));
    db_from_name_text_pairs(files)
}

/// The library half of [`db_from_source_dir_scan`]: builtin.vir,
/// ice40.vir, ...  Used as the degraded fallback when the LSP server
/// cannot read the project's source directory.
pub fn db_from_lib_dir() -> Db {
    db_from_name_text_pairs(load_lib_dir(virdant::LIB_DIR.as_path()))
}

/// Build a self-contained `Db` from an explicit list of `.vir` files
/// (the `-F` flow), plus the standard library.  Explicitly named files
/// are loaded regardless of their stems; the compiler's package-name
/// check reports any that are not valid identifiers.
pub fn db_from_files_with_lib<P, Q>(source_files: Vec<P>, lib_dir: Q) -> Db
where P: Into<PathBuf>, Q: Into<std::path::PathBuf> {
    let mut files = load_lib_dir(lib_dir);

    for source_file in source_files {
        files.push(Source::load_file(source_file.into()));
    }

    db_from_name_text_pairs(files)
}

/// [`db_from_files_with_lib`] with the standard library directory.
pub fn db_from_files<P: Into<PathBuf>>(source_files: Vec<P>) -> Db {
    db_from_files_with_lib(source_files, virdant::LIB_DIR.as_path())
}
