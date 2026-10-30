//! Build script that bakes version information into the `vir` binary.
//!
//! The version string is assembled from three sources:
//!
//! * The version number comes from the `VERSION` file in the base
//!   directory (the repository root).
//! * The commit hash is the 7-hexdigit prefix of the current git
//!   commit, with `-dirty` appended when the working directory has
//!   modifications (tracked files only, mirroring `git describe
//!   --dirty`: untracked files do not count).
//! * The date is the commit date in `date -I` format.
//!
//! The result, e.g. `v0.4.0 (29ace3e 2026-10-29)`, is exposed to the
//! crate as the `VIR_VERSION_STRING` environment variable. Outside a
//! git repository (e.g. in a release tarball), the version is just
//! `v0.4.0`.

use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    let base_dir: PathBuf = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .unwrap()
        .to_owned();

    let version = std::fs::read_to_string(base_dir.join("VERSION"))
        .expect("missing VERSION file in the base directory")
        .trim()
        .to_owned();

    let git_info = git_commit_info(&base_dir);
    let version_string = match git_info {
        Some((hash, dirty, date)) => {
            format!("v{version} ({hash}{} {date})", if dirty { "-dirty" } else { "" })
        }
        None => format!("v{version}"),
    };
    println!("cargo:rustc-env=VIR_VERSION_STRING={version_string}");

    // Rebuild when the version or the checked-out commit changes.
    // (Modifications to already-checked-out files are picked up only
    // by other triggers of a rebuild, such as an ordinary `cargo
    // build` after editing source files.)
    println!("cargo:rerun-if-changed=VERSION");
    println!("cargo:rerun-if-changed=.git/HEAD");
    let head_ref = std::fs::read_to_string(base_dir.join(".git/HEAD")).unwrap_or_default();
    let head_ref = head_ref.trim().strip_prefix("ref: ").unwrap_or("");
    if !head_ref.is_empty() {
        println!("cargo:rerun-if-changed={head_ref}");
    }
}

/// Run `git` in `base_dir`, returning its trimmed standard output, or
/// `None` if `git` is unavailable or the directory is not a git
/// repository.
fn git(base_dir: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(base_dir)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let stdout = String::from_utf8(output.stdout).ok()?;
    Some(stdout.trim().to_owned())
}

/// The `(hash, dirty, date)` of the current commit, where `hash` is
/// the 7-hexdigit prefix of the commit hash, `dirty` is whether the
/// working directory has modifications to tracked files, and `date`
/// is the commit date in `date -I` format.
fn git_commit_info(base_dir: &Path) -> Option<(String, bool, String)> {
    let commit = git(base_dir, &["rev-parse", "HEAD"])?;
    let hash = commit.get(0..7)?.to_owned();
    let dirty = git(base_dir, &["status", "--porcelain", "--untracked-files=no"])
        .map_or(false, |status| !status.is_empty());
    // `--date=short` is `date -I` format (YYYY-MM-DD).
    let date = git(base_dir, &["show", "-s", "--format=%cd", "--date=short", "HEAD"])?;
    Some((hash, dirty, date))
}
