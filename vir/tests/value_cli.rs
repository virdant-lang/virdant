//! End-to-end CLI tests for `vir-value` (and its `vir value`
//! companion-binary wiring).
//!
//! Each test builds a throwaway project (the builtin library plus one
//! scratch package) in a temp directory and runs the binaries with
//! `-F`, the same flow `vir poke` supports.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The scratch package: one of every layout shape.
const SCRATCH: &str = r#"
    pub struct type Pair[T : Type, U : Type] {
        first : T
        second : U
    }

    pub union type Cell[T : Type] {
        Empty
        Full(value : T)
    }

    pub enum type Mode {
        Idle
        Run
    }
"#;

fn builtin_source() -> String {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(root.join("../lib/builtin.vir")).unwrap()
}

/// Build a throwaway project directory; returns (dir, files).
fn project_dir() -> (tempdir::TempDir, Vec<PathBuf>) {
    let dir = tempdir::new();
    let builtin = dir.path().join("builtin.vir");
    let scratch = dir.path().join("scratch.vir");
    std::fs::write(&builtin, builtin_source()).unwrap();
    std::fs::write(&scratch, SCRATCH).unwrap();
    (dir, vec![builtin, scratch])
}

// A tiny tempdir helper (no external dependency): a unique directory
// under the system temp dir, removed on drop.
mod tempdir {
    use std::path::{Path, PathBuf};
    use std::sync::atomic::{AtomicU64, Ordering};

    static COUNTER: AtomicU64 = AtomicU64::new(0);

    pub struct TempDir(PathBuf);

    pub fn new() -> TempDir {
        let n = COUNTER.fetch_add(1, Ordering::SeqCst);
        let path = std::env::temp_dir().join(format!(
            "vir-value-test-{}-{}",
            std::process::id(),
            n
        ));
        std::fs::create_dir_all(&path).unwrap();
        TempDir(path)
    }

    impl TempDir {
        pub fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
}

fn run_value(args: &[&str], files: &[PathBuf]) -> (i32, String, String) {
    let joined = files
        .iter()
        .map(|p| p.to_str().unwrap())
        .collect::<Vec<_>>()
        .join(",");
    let out = Command::new(env!("CARGO_BIN_EXE_vir-value"))
        .args(["-F", &joined])
        .args(args)
        .output()
        .expect("run vir-value");
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn encode_examples() {
    let (_dir, files) = project_dir();
    // `vir value encode @Valid(0) : Valid[Word[8]]`
    let (code, out, err) = run_value(&["encode", "@Valid(0)", ":", "Valid[Word[8]]"], &files);
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "0b0000000010");

    // `vir value encode @Valid(0w8)`
    let (code, out, err) = run_value(&["encode", "@Valid(0w8)"], &files);
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "0b0000000010");

    // The poke-style `#` constructor prefix is accepted too.
    let (code, out, err) = run_value(&["encode", "#Valid(0w8)"], &files);
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "0b0000000010");
}

#[test]
fn encode_cant_infer_type() {
    let (_dir, files) = project_dir();
    let (code, out, err) = run_value(&["encode", "@Valid(0)"], &files);
    assert_eq!(code, 1);
    assert!(out.is_empty());
    assert_eq!(err.trim(), "ERROR: Can't infer type");
}

#[test]
fn decode_examples() {
    let (_dir, files) = project_dir();
    let (code, out, err) = run_value(&["decode", "0b0000000010", "Valid[Word[8]]"], &files);
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "@Valid(0)");

    // Struct round trip through a non-builtin, fully-qualified type.
    let (code, out, err) = run_value(&["encode", "@Full(0w4)"], &files);
    assert_eq!(code, 0, "stderr: {err}");
    // Layout: one-hot tag (10) in the low 2 bits, 4-bit payload above.
    assert_eq!(out.trim(), "0b000010");

    let (code, out, err) = run_value(
        &["decode", "0b000010", "scratch::Cell[Word[4]]"],
        &files,
    );
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "@Full(0)");
}

#[test]
fn decode_enum_examples() {
    let (_dir, files) = project_dir();
    // Zero-payload enum enumerants decode to the `#` spelling.
    let (code, out, err) = run_value(&["decode", "0b01", "scratch::Mode"], &files);
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "#Idle");

    let (code, out, err) = run_value(&["decode", "0b10", "scratch::Mode"], &files);
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "#Run");

    // ... and both spellings round-trip through encode.
    for spelling in &["#Run", "@Run()", "@Run"] {
        let (code, out, err) = run_value(&["encode", spelling, ":", "scratch::Mode"], &files);
        assert_eq!(code, 0, "stderr: {err}");
        assert_eq!(out.trim(), "0b10");
    }
}

#[test]
fn struct_round_trip() {
    let (_dir, files) = project_dir();
    // Named struct literal (the decode output spelling) and the
    // source-style anonymous spelling both encode.
    let (code, out, err) = run_value(
        &["encode", "${Pair(first = 0b1010, second = true)}", ":", "scratch::Pair[Word[4], Bit]"],
        &files,
    );
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "0b11010");

    let (code, out, err) = run_value(
        &["decode", "0b11010", "scratch::Pair[Word[4], Bit]"],
        &files,
    );
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out.trim(), "${Pair(first = 10, second = true)}");
}

#[test]
fn decode_invalid_pattern() {
    let (_dir, files) = project_dir();
    // Unknown tag: 3 is not a known one-hot enumerant value.
    let (code, _out, err) = run_value(&["decode", "0b0000000011", "Valid[Word[8]]"], &files);
    assert_eq!(code, 1);
    assert!(err.contains("unknown tag"), "{err}");
}

#[test]
fn vir_value_companion_discovery() {
    // `vir value` delegates to the `vir-value` companion binary found
    // on `$PATH`, which then resolves the project from the cwd (the
    // `vir` wrapper forwards only the post-subcommand arguments, so
    // `-F` is not part of the companion flow).
    let dir = project_dir_with_manifest();

    // Companion discovery scans `$PATH`; put this tree's build dir
    // (where `vir-value` sits beside `vir`) at the front.
    let exe_dir = Path::new(env!("CARGO_BIN_EXE_vir"))
        .parent()
        .unwrap()
        .to_path_buf();
    let path = std::env::join_paths(
        std::iter::once(exe_dir)
            .chain(std::env::split_paths(&std::env::var_os("PATH").unwrap_or_default())),
    )
    .unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_vir"))
        .args(["value", "encode", "@Valid(0)", ":", "Valid[Word[8]]"])
        .current_dir(dir.path())
        .env("PATH", path)
        .output()
        .expect("run vir value");
    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "0b0000000010");
}

/// A project-shaped temp dir: `virdant.toml` + `src/scratch.vir` (the
/// builtin library is loaded automatically from the standard library
/// directory).
fn project_dir_with_manifest() -> tempdir::TempDir {
    let dir = tempdir::new();
    std::fs::create_dir_all(dir.path().join("src")).unwrap();
    std::fs::write(dir.path().join("virdant.toml"), "").unwrap();
    std::fs::write(dir.path().join("src/scratch.vir"), SCRATCH).unwrap();
    dir
}
