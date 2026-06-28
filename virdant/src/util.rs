use std::path::Path;
use std::sync::Arc;

use crate::common::{Width, WordValue};
use crate::db::Db;
use crate::diagnostics::{Diagnostic, DiagnosticLevel};
use crate::fqn::PackageFqn;
#[cfg(not(target_arch = "wasm32"))]
use crate::common::source::Source;

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Db {
    let mut db = Db::new();
    db.set_packages(vec![]);
    let builtin_source = Source::load_file(crate::LIB_DIR.join("builtin.vir"));
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

#[cfg(not(target_arch = "wasm32"))]
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
    let mut db = Db::new();
    db.set_packages(vec![]);

    let lib_dir = lib_dir.into();
    let lib_dir = std::fs::canonicalize(&lib_dir).expect(&format!("Could not find {lib_dir:?}"));

    let builtin_source = Source::load_file(lib_dir.join("builtin.vir"));
    let builtin_package = builtin_source.package();
    db.set_source(builtin_package.clone(), builtin_source);

    let mut packages = vec![builtin_package];

    for source_file in source_files {
        let source = Source::load_file(source_file.into());
        let package = source.package();
        db.set_source(package.clone(), source);
        packages.push(package);
    }

    db.set_packages(packages);

    db
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

#[cfg(not(target_arch = "wasm32"))]
pub fn resolve_platform_path_in(project_root: &Path, platform: &str) -> std::path::PathBuf {
    let local = project_root.join("platform").join(format!("{platform}.vir"));
    if local.is_file() {
        return std::fs::canonicalize(&local)
            .expect(&format!("Could not canonicalize platform path: {local:?}"));
    }
    let global = crate::PLATFORMS_DIR.join(format!("{platform}.vir"));
    if global.is_file() {
        return global;
    }
    panic!("platform file not found for: {platform}");
}

#[cfg(not(target_arch = "wasm32"))]
pub fn resolve_platform_path(platform: &str) -> std::path::PathBuf {
    resolve_platform_path_in(Path::new("."), platform)
}

pub fn read_platform_from_toml(source_dir: &Path) -> Result<Option<String>, String> {
    let toml_path = source_dir.join("Virdant.toml");
    if !toml_path.exists() {
        return Ok(None);
    }
    let contents = std::fs::read_to_string(&toml_path)
        .map_err(|e| format!("failed to read Virdant.toml: {e}"))?;
    let value: toml::Value = toml::from_str(&contents)
        .map_err(|e| format!("failed to parse Virdant.toml: {e}"))?;
    Ok(value.get("prog").and_then(|p| p.get("platform"))
        .and_then(|s| s.as_str()).map(|s| s.to_owned()))
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir_with_platform<P: Into<std::path::PathBuf>>(
    source_dir: P,
    platform: &str,
) -> Db {
    db_from_dir_with_platform_and_lib(source_dir, platform, crate::LIB_DIR.as_path())
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir_with_platform_and_lib<P, Q>(
    source_dir: P,
    platform: &str,
    lib_dir: Q,
) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf>,
{
    let mut db = db_from_dir_with_lib(source_dir, lib_dir);
    let path = resolve_platform_path(platform);
    let pkg: PackageFqn = platform.to_owned().into();
    let source = Source::load_file(path);
    db.set_source(pkg.clone(), source);
    let mut packages: Vec<PackageFqn> = (*db.get_packages()).clone();
    if !packages.contains(&pkg) {
        packages.push(pkg);
        db.set_packages(packages);
    }
    db
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir_with_platform_in<P: Into<std::path::PathBuf>>(
    source_dir: P,
    project_root: &Path,
    platform: &str,
) -> Db {
    db_from_dir_with_platform_and_lib_in(source_dir, project_root, platform, crate::LIB_DIR.as_path())
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_dir_with_platform_and_lib_in<P, Q>(
    source_dir: P,
    project_root: &Path,
    platform: &str,
    lib_dir: Q,
) -> Db
where P: Into<std::path::PathBuf>, Q: Into<std::path::PathBuf>,
{
    let mut db = db_from_dir_with_lib(source_dir, lib_dir);
    let path = resolve_platform_path_in(project_root, platform);
    let pkg: PackageFqn = platform.to_owned().into();
    let source = Source::load_file(path);
    db.set_source(pkg.clone(), source);
    let mut packages: Vec<PackageFqn> = (*db.get_packages()).clone();
    if !packages.contains(&pkg) {
        packages.push(pkg);
        db.set_packages(packages);
    }
    db
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_files_with_platform(
    source_files: Vec<std::path::PathBuf>,
    platform: &str,
) -> Db {
    db_from_files_with_platform_and_lib(source_files, platform, crate::LIB_DIR.as_path())
}

#[cfg(not(target_arch = "wasm32"))]
pub fn db_from_files_with_platform_and_lib<Q>(
    source_files: Vec<std::path::PathBuf>,
    platform: &str,
    lib_dir: Q,
) -> Db
where Q: Into<std::path::PathBuf>,
{
    let mut db = db_from_files_with_lib(source_files, lib_dir);
    let path = resolve_platform_path(platform);
    let pkg: PackageFqn = platform.to_owned().into();
    let source = Source::load_file(path);
    db.set_source(pkg.clone(), source);
    let mut packages: Vec<PackageFqn> = (*db.get_packages()).clone();
    if !packages.contains(&pkg) {
        packages.push(pkg);
        db.set_packages(packages);
    }
    db
}
