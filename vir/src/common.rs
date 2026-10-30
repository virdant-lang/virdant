//! Shared helpers for the `vir` command-line binaries: resolving the
//! project location (`-C`/`-F`), building the analysis `Db`, printing
//! diagnostics, and maintaining the `build/` directory.
//!
//! Each binary includes this module with `#[path = "../common.rs"]`.
//! (`help_json.rs` lives beside it under `common/` and is included
//! separately, since not every binary links the compiler.)

use std::path::{Path, PathBuf};

use clap::{Arg, ArgMatches, Command as ClapCommand, value_parser};
use colored::Colorize;

use virdant::common::source::Region;
use virdant::db::Db;
use virdant::diagnostics::DiagnosticLevel;
use virdant::util::{check_db, db_from_dir, db_from_files};

// ---------------------------------------------------------------------------
// Project manifest (`virdant.toml`)
// ---------------------------------------------------------------------------

/// The project manifest filename.  `virdant.toml` is still accepted, but
/// deprecated; [`project_manifest`] emits a warning when it is used.
pub const PROJECT_MANIFEST: &str = "virdant.toml";

/// Whether the `Virdant.toml` deprecation warning has been printed in
/// this process; the warning is emitted at most once.
static MANIFEST_DEPRECATION_WARNED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Resolve the project manifest in `dir`: `virdant.toml` if present,
/// else the deprecated `Virdant.toml` (with a deprecation warning to
/// stderr, printed at most once per process).  Returns `None` when
/// neither exists.
pub fn project_manifest(dir: &Path) -> Option<PathBuf> {
    use std::sync::atomic::Ordering;

    let path = dir.join(PROJECT_MANIFEST);
    if path.exists() {
        return Some(path);
    }
    let legacy = dir.join("Virdant.toml");
    if legacy.exists() {
        if !MANIFEST_DEPRECATION_WARNED.swap(true, Ordering::Relaxed) {
            eprintln!(
                "Warning: 'Virdant.toml' is deprecated; rename it to 'virdant.toml'."
            );
        }
        return Some(legacy);
    }
    None
}

/// Resolve the project manifest in `dir`, exiting with an error when
/// neither `virdant.toml` nor the deprecated `Virdant.toml` exists.
pub fn project_manifest_or_exit(dir: &Path) -> PathBuf {
    match project_manifest(dir) {
        Some(path) => path,
        None => {
            eprintln!("No virdant.toml found");
            std::process::exit(1);
        }
    }
}

// ---------------------------------------------------------------------------
// Project location (`-C` / `-F`)
// ---------------------------------------------------------------------------

/// Add the project-location args (`-C/--cwd`, `-F/--virfile`) shared by
/// the `vir` binaries to a command under construction.
///
/// If `hide_virfile` is true, the `-F/--virfile` arg is hidden from help
/// listings (it remains fully functional).
pub fn add_project_args(cmd: &mut ClapCommand, hide_virfile: bool) {
    *cmd = std::mem::take(cmd)
        .arg(
            Arg::new("cwd")
                .short('C')
                .value_name("CWD")
                .value_parser(value_parser!(PathBuf))
                .conflicts_with("virfile"),
        )
        .arg(
            Arg::new("virfile")
                .short('F')
                .value_name("VIRFILE")
                .value_parser(value_parser!(String))
                .conflicts_with("cwd")
                .hide(hide_virfile)
                .help("Load .vir file(s) as a self-contained project (comma-separated, no spaces; exclusive with -C)"),
        );
}

/// The project location parsed from the command line: an explicit
/// working directory (`-C`), a list of standalone `.vir` files (`-F`),
/// or neither.
#[derive(Debug, Clone, Default)]
pub struct ProjectLocation {
    pub cwd: Option<PathBuf>,
    pub virfile: Option<String>,
}

impl ProjectLocation {
    /// Extract `-C`/`-F` from parsed [`ArgMatches`].
    pub fn from_matches(matches: &ArgMatches) -> Self {
        ProjectLocation {
            cwd: matches.get_one::<PathBuf>("cwd").cloned(),
            virfile: matches.get_one::<String>("virfile").cloned(),
        }
    }

    /// Canonicalize `cwd` if present, else the current directory.
    pub fn resolve_cwd(&self) -> PathBuf {
        if let Some(cwd) = &self.cwd {
            match std::fs::canonicalize(cwd) {
                Ok(path) => path,
                Err(e) => {
                    eprintln!("ERROR: cannot resolve directory {}: {e}", cwd.display());
                    std::process::exit(1);
                }
            }
        } else {
            match std::env::current_dir() {
                Ok(path) => path,
                Err(e) => {
                    eprintln!("ERROR: cannot determine current directory: {e}");
                    std::process::exit(1);
                }
            }
        }
    }

    /// Build the analysis `Db` for the project: `-F` file(s), else
    /// `src/` under the resolved working directory.
    pub fn project_db(&self) -> Db {
        if let Some(virfile) = &self.virfile {
            let paths: Vec<std::path::PathBuf> = virfile
                .split(',')
                .map(|s| std::path::PathBuf::from(s.trim()))
                .collect();
            for path in &paths {
                if !path.is_file() {
                    eprintln!("ERROR: file not found: {}", path.display());
                    std::process::exit(1);
                }
            }
            return db_from_files(paths);
        }

        let cwd = self.resolve_cwd();

        project_manifest_or_exit(&cwd);

        let src_dir = cwd.join("src");
        if !src_dir.is_dir() {
            eprintln!("ERROR: source directory not found: {}", src_dir.display());
            std::process::exit(1);
        }

        db_from_dir(src_dir)
    }
}

/// Read `[prog].<key>` from `virdant.toml` in `cwd`, tolerating a
/// missing file or missing keys.
pub fn read_prog_key(cwd: &Path, key: &str) -> Option<String> {
    read_prog_key_from(&project_manifest(cwd)?, key)
}

/// Like [`read_prog_key`], but reads from an already-resolved manifest
/// path (so the deprecation warning is not re-emitted).
pub fn read_prog_key_from(manifest: &Path, key: &str) -> Option<String> {
    let text = std::fs::read_to_string(manifest).ok()?;
    let toml: toml::Value = toml::from_str(&text).ok()?;
    toml.get("prog")
        .and_then(|prog| prog.get(key))
        .and_then(|value| value.as_str())
        .map(|s| s.to_owned())
}

/// Read `[project].name` from `virdant.toml` in `cwd`.
pub fn read_project_name(cwd: &Path) -> Option<String> {
    read_project_name_from(&project_manifest(cwd)?)
}

/// Like [`read_project_name`], but reads from an already-resolved
/// manifest path (so the deprecation warning is not re-emitted).
pub fn read_project_name_from(manifest: &Path) -> Option<String> {
    let text = std::fs::read_to_string(manifest).ok()?;
    let toml: toml::Value = toml::from_str(&text).ok()?;
    toml.get("project")
        .and_then(|project| project.get("name"))
        .and_then(|value| value.as_str())
        .map(|s| s.to_owned())
}

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/// Print one diagnostic in the same style as [`dump_diagnostics`].
pub fn dump_diagnostic(db: &Db, diagnostic: &virdant::diagnostics::Diagnostic) {
    println!(
        "{}   {}   {}",
        "ERROR  ".red(),
        region_string(db, diagnostic.region()),
        diagnostic.message(),
    );
}

pub fn region_string(db: &Db, region: Region) -> String {
    use bstr::ByteSlice as _;

    let package = db.get_packages().name(region.package()).to_str_lossy().into_owned();
    let span = region.span().to_string();

    format!("{package}.vir {span}")
}

pub fn dump_diagnostics(db: &Db) {
    let diagnostics = match check_db(db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };
    let longest_region = diagnostics
        .iter()
        .map(|diag| region_string(db, diag.region()).len())
        .max()
        .unwrap_or_default();

    let mut warning_count = 0;
    let mut error_count = 0;

    for diagnostic in diagnostics.iter() {
        let unpadded_region = region_string(db, diagnostic.region());
        let padded_region = format!("{}{}", unpadded_region, " ".repeat(longest_region - unpadded_region.len()));
        if diagnostic.level() == DiagnosticLevel::Error {
            println!("{}   {}   {}", "ERROR  ".red(), padded_region, diagnostic.message());
            error_count += 1;
        } else if diagnostic.level() == DiagnosticLevel::Warning {
            println!("{}   {}   {}", "WARNING".yellow(), padded_region, diagnostic.message());
            warning_count += 1;
        } else {
            println!("{}   {}   {}", "INFO   ".green(), padded_region, diagnostic.message());
        }
    }

    let failed = error_count > 0;

    if error_count > 0 || warning_count > 0 {
        println!();
        if failed {
            println!("{} with:", "FAILED".red());
        } else {
            println!("{} with:", "PASSED".green());
        }
        if error_count > 0 {
            println!("{error_count:>4} {}", "ERROR".red());
        }

        if warning_count > 0 {
            println!("{warning_count:>4} {}", "WARNING".yellow());
        }
    }
}

/// Render the project's diagnostics as a JSON array.
pub fn diagnostics_json(db: &Db) -> json::JsonValue {
    let diagnostics = match check_db(db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };
    let mut array = json::JsonValue::new_array();
    for diagnostic in diagnostics.iter() {
        let mut entry = json::object::Object::new();
        let level = match diagnostic.level() {
            DiagnosticLevel::Error => "error",
            DiagnosticLevel::Warning => "warning",
            DiagnosticLevel::Info => "info",
        };
        entry.insert("level", level.into());
        entry.insert("region", region_string(db, diagnostic.region()).into());
        entry.insert("message", diagnostic.message().to_string().into());
        array.push(json::JsonValue::Object(entry)).unwrap();
    }
    array
}

// ---------------------------------------------------------------------------
// Build directory
// ---------------------------------------------------------------------------

/// Mirror the project's `ext/` directory (if any) into `build/ext/`.
///
/// `ext mod` bodies live outside Virdant as plain Verilog under `ext/`,
/// so a build directory holding only emitted Verilog cannot synthesize
/// or simulate a design that instantiates one.  Mirroring `ext/` into
/// the build directory puts the external sources next to the emitted
/// Verilog, where the `.sv` globbers used by yosys and iverilog pick
/// them up, and keeps data files (e.g. `$readmemh` images) reachable
/// at their original `ext/...` relative paths once cwd moves into
/// `build/`.
pub fn copy_ext_dir(builddir: &Path) {
    let Some(projectdir) = builddir.parent() else { return };
    let extdir = projectdir.join("ext");
    if !extdir.is_dir() {
        return;
    }
    // Start from a clean slate so ext files removed since the last
    // build do not linger in the mirror.
    let target = builddir.join("ext");
    if target.exists() {
        std::fs::remove_dir_all(&target).unwrap();
    }
    if let Err(err) = copy_tree(&extdir, &target) {
        eprintln!("Could not copy {} to {}: {err}", extdir.display(), target.display());
        std::process::exit(1);
    }
    println!("Copied {} to {}", extdir.display(), target.display());
}

pub fn copy_tree(source: &Path, target: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(target)?;
    for entry in std::fs::read_dir(source)? {
        let entry = entry?;
        let path = entry.path();
        let dest = target.join(entry.file_name());
        if path.is_dir() {
            copy_tree(&path, &dest)?;
        } else {
            std::fs::copy(&path, &dest)?;
        }
    }
    Ok(())
}

/// Collect all `.sv` files under `dir`, recursively, sorted.
pub fn glob_sv_files(dir: &Path) -> Vec<PathBuf> {
    let mut sources = vec![];
    collect_sv_files(dir, &mut sources);
    sources.sort();
    sources
}

fn collect_sv_files(dir: &Path, sources: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = std::fs::canonicalize(entry.unwrap().path()).unwrap();
        if path.is_dir() {
            collect_sv_files(&path, sources);
        } else if path.extension().is_some_and(|ext| ext == "sv") {
            sources.push(path);
        }
    }
}

// ---------------------------------------------------------------------------
// Shared match-arm helpers (clap value extraction)
// ---------------------------------------------------------------------------

pub fn matches_string(matches: &ArgMatches, id: &str) -> String {
    matches.get_one::<String>(id).cloned().unwrap()
}

pub fn matches_path(matches: &ArgMatches, id: &str) -> PathBuf {
    matches.get_one::<PathBuf>(id).cloned().unwrap()
}

pub fn matches_opt_string(matches: &ArgMatches, id: &str) -> Option<String> {
    matches.get_one::<String>(id).cloned()
}

pub fn matches_opt_path(matches: &ArgMatches, id: &str) -> Option<PathBuf> {
    matches.get_one::<PathBuf>(id).cloned()
}

pub fn matches_flag(matches: &ArgMatches, id: &str) -> bool {
    matches.get_flag(id)
}

// ---------------------------------------------------------------------------
