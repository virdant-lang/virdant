//! The host variant: the standard library installed as files, found
//! relative to the running executable.
//!
//! The WASI mirror of this module is [`crate::stdlib::wasm32`], which
//! embeds the same files; the two must expose exactly the same API.

use std::path::PathBuf;

use bstr::BStr;
use bstr::BString;
use bstr::ByteSlice as _;

/// The installed library directory: `lib/` two levels above the
/// executable (`target/release/../lib`; `${HOME}/.local/virdant/lib`
/// after `make install`).  Panics when it cannot be found.
fn lib_dir() -> PathBuf {
    let exe = std::env::current_exe().unwrap();
    let root = exe.parent().unwrap().parent().unwrap();
    let lib = root.join("lib");
    std::fs::canonicalize(&lib).expect(&format!("Could not find {lib:?}"))
}
/// The file for stdlib package `name`, if it exists on disk
/// (e.g. for goto-definition into `ice40::...`).  Always `None` on
/// `wasm32` (see `stdlib::wasm32`).
pub fn package_path(name: &BStr) -> Option<PathBuf> {
    let path = lib_dir().join(format!("{}.vir", name.to_str_lossy()));
    path.exists().then_some(path)
}

/// The package name a `.vir` file is loaded as: its file stem.
fn package_name_of(filepath: &std::path::Path) -> BString {
    BString::from(filepath.file_stem().unwrap_or_default().as_encoded_bytes())
}

/// The standard library as `(package name, source text)` pairs, in
/// deterministic (sorted) order.
///
/// The standard library ships with the compiler; a bad name there is
/// a bug in Virdant itself, not a user error, so invalid names panic.
pub fn sources() -> Vec<(BString, BString)> {
    let lib_dir = lib_dir();
    let mut filepaths: Vec<std::path::PathBuf> = std::fs::read_dir(&lib_dir)
        .expect(&format!("Could not open directory: {lib_dir:?}"))
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|filepath| match filepath.extension() {
            Some(ext) => ext.to_string_lossy() == "vir",
            None => false,
        })
        .collect();
    filepaths.sort();
    for filepath in &filepaths {
        assert!(
            crate::util::is_valid_package_name(package_name_of(filepath).as_bstr()),
            "Invalid package name in standard library: {}",
            filepath.display(),
        );
    }
    filepaths
        .into_iter()
        .map(crate::common::source::Source::load_file)
        .collect()
}
