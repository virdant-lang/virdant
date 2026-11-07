//! Whole-project loading for the language queries.

use std::path::{Path, PathBuf};

use bstr::ByteSlice as _;

use virdant::db::Db;
use virdant::package::PackageId;

/// A whole-project query context: the analysis `Db` plus the directory
/// the project's `*.vir` files were loaded from.  The source directory
/// doubles as the package-to-file mapping used by the CLI's JSON output
/// (a package named `top` loaded from `<dir>` lives in `<dir>/top.vir`).
pub struct Project {
    pub db: Db,
    pub source_dir: PathBuf,
}

impl Project {
    /// Standard project discovery, like the other `vir` commands:
    /// `virdant.toml` (or the deprecated `Virdant.toml`) must exist in
    /// `dir` and the sources live in `dir/src`.  The `Db` covers the
    /// builtin library plus every project `*.vir` file.
    pub fn from_project_dir(dir: &Path) -> Result<Project, String> {
        // Use the shared manifest lookup so the deprecated
        // `Virdant.toml` gets its deprecation warning too.
        if vircommon::project_manifest(dir).is_none() {
            return Err(format!("No virdant.toml found in {}", dir.display()));
        }
        let src_dir = dir.join("src");
        if !src_dir.is_dir() {
            return Err(format!("No src directory in {}", dir.display()));
        }
        let db = db_from_dir(src_dir.clone())?;
        Ok(Project {
            db,
            source_dir: src_dir,
        })
    }

    /// Ad-hoc source directory (no manifest): every `*.vir` directly in
    /// `dir` is loaded alongside the builtin library.  Used when a CLI
    /// query names a `.vir` file whose parent directory is not a
    /// manifest project, matching the pre-existing `vir ls hover`
    /// behavior.
    pub fn from_source_dir(dir: &Path) -> Result<Project, String> {
        if !dir.is_dir() {
            return Err(format!("Not a directory: {}", dir.display()));
        }
        let db = db_from_dir(dir.to_path_buf())?;
        Ok(Project {
            db,
            source_dir: dir.to_path_buf(),
        })
    }

    /// The canonical JSON spelling of a package's `file` field:
    /// environment-independent.  Project files are rendered relative to
    /// the project directory (`src/top.vir`), so the same `vir ls` JSON
    /// (and the golden tests pinning it) is byte-identical no matter
    /// where the project or the repository live on disk.  Library
    /// packages (builtin, ice40, ...) are tagged `<lib>/name.vir`.
    pub fn file_json_path(&self, package: PackageId) -> String {
        let name = self.db.get_packages().name(package).to_str_lossy().into_owned();
        let source_path = self.source_dir.join(format!("{name}.vir"));
        if !source_path.exists() {
            // Library package: the file lives in the standard library.
            return format!("<lib>/{name}.vir");
        }
        // Project package: render relative to the project directory.
        if let Some(project_dir) = self.source_dir.parent() {
            if let Ok(relative) = source_path.strip_prefix(project_dir) {
                return relative.to_string_lossy().into_owned();
            }
        }
        source_path.to_string_lossy().into_owned()
    }
}

fn db_from_dir(source_dir: std::path::PathBuf) -> Result<Db, String> {
    // Strict CLI handling: an unreadable source directory, or a `.vir`
    // file whose name is not a valid package identifier, is a tool
    // failure (the caller turns it into exit 2).  The LSP server has
    // its own warn-and-continue wrapper around the same
    // `virdant::util::scan_source_dir` scan, which skips bad-named
    // files instead of failing the load.
    vircommon::db_from_dir(source_dir)
}
