//! Test discovery and the `test.toml` format.
//!
//! A test is any directory under `tests/` containing a `test.toml`
//! file.  The test's stable name is its directory path, starting with
//! `tests/` (e.g. `tests/errors/typing/word_literals`).  The same name
//! is used on screen and under `build/tests/`.

use std::path::{Path, PathBuf};

use serde::Deserialize;

/// Everything the runner needs to know about one test.
#[derive(Debug, Clone)]
pub struct Test {
    /// Stable name: `tests/path/to/test`.
    pub name: String,
    /// Directory holding `test.toml`, relative to the repo root.
    pub dir: PathBuf,
    /// One-line description shown next to the name.
    pub description: String,
    /// Skip reason, if the test is deliberately skipped.
    pub skip: Option<String>,
    pub kind: Kind,
    pub check_cfg: CheckCfg,
    pub sim_cfg: SimCfg,
    pub format_cfg: FormatCfg,
    pub golden_cfg: GoldenCfg,
    pub doc_cfg: DocCfg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// `vir <command>` on a project, with a `filecheck` contract.
    Check,
    /// `vir build` + iverilog / verilator simulation.
    Sim,
    /// `vir format` compared against a golden file.
    Format,
    /// `vir build` output compared against a reference.
    Golden,
    /// `vir doc` output assertions.
    Doc,
}

impl Kind {
    pub fn as_str(self) -> &'static str {
        match self {
            Kind::Check => "check",
            Kind::Sim => "sim",
            Kind::Format => "format",
            Kind::Golden => "golden",
            Kind::Doc => "doc",
        }
    }
}

/// `expect = ...` exit-code/output contract for `kind = "check"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expect {
    /// Must exit 0 (plain project check).
    Ok,
    /// Must exit 0 and emit no `WARNING` lines.
    OkNoWarnings,
    /// Must exit 0 and emit at least one `WARNING` line.
    OkWithWarnings,
    /// Must exit nonzero.
    Error,
}

#[derive(Debug, Clone)]
pub struct CheckCfg {
    /// `vir <command>` (default `check`).
    pub command: String,
    pub expect: Expect,
    /// File holding CHECK directives, relative to the test directory.
    pub filecheck: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct SimCfg {
    pub simulator: Simulator,
    pub flags: Vec<String>,
    /// Sources passed to iverilog (default `["build/*.sv", "tb.sv"]`).
    pub sources: Option<Vec<String>>,
    /// verilator only: pass `--lint-only` (default true for verilator).
    pub lint_only: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Simulator {
    Icarus,
    Verilator,
}

impl Simulator {
    pub fn binary(self) -> &'static str {
        match self {
            Simulator::Icarus => "iverilog",
            Simulator::Verilator => "verilator",
        }
    }
}

#[derive(Debug, Clone)]
pub struct FormatCfg {
    pub input: PathBuf,
    pub golden: PathBuf,
}

#[derive(Debug, Clone)]
pub struct Grep {
    pub path: PathBuf,
    pub pattern: String,
}

#[derive(Debug, Clone)]
pub struct Diff {
    pub a: PathBuf,
    pub b: PathBuf,
    /// Skip the first N lines of both files before comparing
    /// (`tail -n +<N+1>` semantics; default 0).
    pub skip_lines: usize,
}

#[derive(Debug, Clone)]
pub struct GoldenCfg {
    pub grep: Vec<Grep>,
    pub diff: Vec<Diff>,
}

#[derive(Debug, Clone)]
pub struct DocCfg {
    pub expect_files: Vec<PathBuf>,
    pub grep: Vec<Grep>,
}

/// A minimal `Test` for unit tests.  Never built by discovery.
#[cfg(test)]
pub(crate) fn test_stub(name: &str, description: &str) -> Test {
    Test {
        name: name.to_string(),
        dir: std::path::PathBuf::from(name),
        description: description.to_string(),
        skip: None,
        kind: Kind::Check,
        check_cfg: CheckCfg {
            command: "check".to_string(),
            expect: Expect::Ok,
            filecheck: None,
        },
        sim_cfg: SimCfg {
            simulator: Simulator::Icarus,
            flags: vec![],
            sources: None,
            lint_only: false,
        },
        format_cfg: FormatCfg {
            input: std::path::PathBuf::from("input.vir"),
            golden: std::path::PathBuf::from("golden.vir"),
        },
        golden_cfg: GoldenCfg { grep: vec![], diff: vec![] },
        doc_cfg: DocCfg { expect_files: vec![], grep: vec![] },
    }
}

// --- test.toml deserialization ------------------------------------------------

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TestToml {
    kind: String,
    description: Option<String>,
    #[serde(default)]
    skip: Option<SkipField>,
    // kind = "check"
    command: Option<String>,
    expect: Option<String>,
    filecheck: Option<PathBuf>,
    // kind = "sim"
    simulator: Option<String>,
    flags: Option<Vec<String>>,
    sources: Option<Vec<String>>,
    lint_only: Option<bool>,
    // kind = "format"
    input: Option<PathBuf>,
    golden: Option<PathBuf>,
    // kind = "golden" / "doc"
    #[serde(default)]
    grep: Vec<GrepToml>,
    #[serde(default)]
    diff: Vec<DiffToml>,
    // kind = "doc"
    #[serde(default)]
    expect_files: Vec<PathBuf>,
}

/// `skip = true` or `skip = "reason"`.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum SkipField {
    Bool(bool),
    Reason(String),
}

#[derive(Debug, Clone, Deserialize)]
struct GrepToml {
    path: PathBuf,
    pattern: String,
}

#[derive(Debug, Clone, Deserialize)]
struct DiffToml {
    a: PathBuf,
    b: PathBuf,
    skip_lines: Option<usize>,
}

impl From<GrepToml> for Grep {
    fn from(g: GrepToml) -> Grep {
        Grep { path: g.path, pattern: g.pattern }
    }
}

impl From<DiffToml> for Diff {
    fn from(d: DiffToml) -> Diff {
        Diff { a: d.a, b: d.b, skip_lines: d.skip_lines.unwrap_or(0) }
    }
}

pub fn parse_test_toml(dir: &Path, text: &str) -> Result<Test, String> {
    let raw: TestToml = toml::from_str(text).map_err(|e| {
        format!("{}: invalid test.toml: {}", dir.join("test.toml").display(), e)
    })?;

    let kind = match raw.kind.as_str() {
        "check" => Kind::Check,
        "sim" => Kind::Sim,
        "format" => Kind::Format,
        "golden" => Kind::Golden,
        "doc" => Kind::Doc,
        other => {
            return Err(format!(
                "{}: unknown kind {:?} (use check | sim | format | golden | doc)",
                dir.join("test.toml").display(),
                other
            ))
        }
    };

    let skip = match raw.skip {
        None => None,
        Some(SkipField::Bool(true)) => Some(String::new()),
        Some(SkipField::Bool(false)) => None,
        Some(SkipField::Reason(reason)) => Some(reason),
    };

    let check_cfg = CheckCfg {
        command: raw.command.clone().unwrap_or_else(|| "check".to_string()),
        expect: match raw.expect.as_deref() {
            None | Some("ok") => Expect::Ok,
            Some("ok_no_warnings") => Expect::OkNoWarnings,
            Some("ok_with_warnings") => Expect::OkWithWarnings,
            Some("error") => Expect::Error,
            Some(other) => {
                return Err(format!(
                    "{}: unknown expect {:?} (use ok | ok_no_warnings | ok_with_warnings | error)",
                    dir.join("test.toml").display(),
                    other
                ))
            }
        },
        filecheck: raw.filecheck.clone(),
    };

    let sim_cfg = SimCfg {
        simulator: match raw.simulator.as_deref() {
            None => Simulator::Icarus,
            Some("iverilog") => Simulator::Icarus,
            Some("verilator") => Simulator::Verilator,
            Some(other) => {
                return Err(format!(
                    "{}: unknown simulator {:?} (use iverilog | verilator)",
                    dir.join("test.toml").display(),
                    other
                ))
            }
        },
        flags: raw.flags.clone().unwrap_or_default(),
        sources: raw.sources.clone(),
        lint_only: raw.lint_only.unwrap_or(true),
    };

    let format_cfg = FormatCfg {
        input: raw.input.clone().unwrap_or_else(|| PathBuf::from("input.vir")),
        golden: raw.golden.clone().unwrap_or_else(|| PathBuf::from("golden.vir")),
    };

    let golden_cfg = GoldenCfg {
        grep: raw.grep.iter().cloned().map(Into::into).collect(),
        diff: raw.diff.iter().cloned().map(Into::into).collect(),
    };

    let doc_cfg = DocCfg {
        expect_files: raw.expect_files.clone(),
        grep: raw.grep.iter().cloned().map(Into::into).collect(),
    };

    Ok(Test {
        name: dir.to_string_lossy().to_string(),
        dir: dir.to_path_buf(),
        description: raw.description.unwrap_or_default(),
        skip,
        kind,
        check_cfg,
        sim_cfg,
        format_cfg,
        golden_cfg,
        doc_cfg,
    })
}

impl Test {
    pub fn needs_simulator(&self) -> Option<&'static str> {
        match self.kind {
            Kind::Sim => Some(self.sim_cfg.simulator.binary()),
            _ => None,
        }
    }
}

/// Find every test in the project: any directory (at any depth below
/// the repository root) containing a `test.toml`.  Discovery order is
/// canonical (tree-sorted).  Build trees and hidden directories are
/// never searched.
pub fn discover(root: &Path) -> Result<Vec<Test>, String> {
    let mut dirs = Vec::new();
    walk(root, &mut dirs)?;

    let mut tests = Vec::new();
    for abs_dir in dirs {
        let toml_path = abs_dir.join("test.toml");
        let text = std::fs::read_to_string(&toml_path)
            .map_err(|e| format!("{}: {}", toml_path.display(), e))?;
        let dir = abs_dir.strip_prefix(root).unwrap().to_path_buf();
        let mut test = parse_test_toml(&abs_dir, &text)?;
        // Stable identity: the directory path relative to the repo root
        // (tests/check/whens, docs/mytest, ...), used identically on
        // screen and in build/tests/ paths.
        test.dir = dir.clone();
        test.name = dir.to_string_lossy().to_string();
        tests.push(test);
    }
    tests.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(tests)
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Ok(()); // unreadable (permissions, race): skip silently
    };
    let mut entries: Vec<_> = entries.filter_map(|entry| entry.ok()).collect();
    entries.sort_by_key(|entry| entry.file_name());

    // A directory containing test.toml is a test; its own subdirectories
    // are part of the test (its sources, its scratch space), not more
    // tests.
    let has_test_toml = entries.iter().any(|entry| entry.file_name() == "test.toml");
    if has_test_toml {
        out.push(dir.to_path_buf());
    }

    for entry in entries {
        let path = entry.path();
        if !path.is_dir() || has_test_toml {
            continue;
        }
        let name = entry.file_name();
        let name = name.to_string_lossy();
        // Hidden directories (.git, .github, .agents, ...) and build
        // trees (target, build) are never searched.
        if name.starts_with('.') || name == "target" || name == "build" {
            continue;
        }
        walk(&path, out)?;
    }
    Ok(())
}
