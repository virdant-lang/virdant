//! Running a single test as subprocesses, with the same commands and
//! exit-code contracts the per-directory Makefiles used before the runner
//! replaced them.

use std::io::Write;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use crate::discovery::{Expect, Kind, Simulator, Test};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    Pass,
    Fail,
    Skip,
}

impl Status {
    pub fn symbol(self) -> &'static str {
        match self {
            Status::Pass => "✅",
            Status::Fail => "❌",
            Status::Skip => "⚠️",
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Status::Pass => "pass",
            Status::Fail => "FAIL",
            Status::Skip => "skip",
        }
    }
}

/// One subprocess (or in-process assertion) recorded for the evidence log.
#[derive(Debug, Clone)]
pub struct StepResult {
    pub command: String,
    pub code: Option<i32>,
    pub signal: Option<i32>,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
    pub duration: Duration,
}

impl StepResult {
    /// stdout ++ stderr, byte-compatible with `> out 2>&1` redirection.
    pub fn merged(&self) -> Vec<u8> {
        let mut merged = self.stdout.clone();
        merged.extend_from_slice(&self.stderr);
        merged
    }

    pub fn exit_description(&self) -> String {
        match (self.code, self.signal) {
            (Some(code), _) => format!("exit {code}"),
            (None, Some(sig)) => format!("killed by signal {sig}"),
            (None, None) => "exit ?".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Outcome {
    pub status: Status,
    /// Why the test failed (concise, for -ff and summaries).
    pub failure: Option<String>,
    /// filecheck stderr, when the filecheck step failed.
    pub filecheck_stderr: Option<String>,
    pub steps: Vec<StepResult>,
    pub duration: Duration,
}

impl Outcome {
    pub fn skip() -> Outcome {
        Outcome {
            status: Status::Skip,
            failure: None,
            filecheck_stderr: None,
            steps: Vec::new(),
            duration: Duration::from_secs(0),
        }
    }
}

/// Context needed to run tests.
pub struct Env {
    /// Repository root (absolute).
    pub root: PathBuf,
    /// Absolute path of the `vir` binary under test.
    pub vir: PathBuf,
}

impl Env {
    fn test_dir(&self, test: &Test) -> PathBuf {
        self.root.join(&test.dir)
    }

    fn log_dir(&self, test: &Test) -> PathBuf {
        // build/tests/<path>/<to>/<test>/ mirrors the test name
        // (build/tests/check/whens for tests/check/whens).
        let rel = test.name.strip_prefix("tests/").unwrap_or(&test.name);
        self.root.join("build").join("tests").join(rel)
    }
}

pub fn run_test(env: &Env, test: &Test) -> Outcome {
    if let Some(_reason) = &test.skip {
        return Outcome::skip();
    }
    let start = Instant::now();
    let outcome = match test.kind {
        Kind::Check => run_check(env, test),
        Kind::Sim => run_sim(env, test),
        Kind::Format => run_format(env, test),
        Kind::Golden => run_golden(env, test),
        Kind::Doc => run_doc(env, test),
    };
    Outcome { duration: start.elapsed(), ..outcome }
}

// --- subprocess plumbing -------------------------------------------------------

/// Run `argv` in `cwd`, capturing stdout and stderr separately.
/// `argv[0]` may be `./relative`; anything else is looked up on PATH
/// (or used as-is when it contains a `/`).
fn run_command(argv: &[String], cwd: &Path, extra_path: Option<&Path>) -> StepResult {
    let start = Instant::now();
    let mut cmd = Command::new(&argv[0]);
    cmd.args(&argv[1..]);
    cmd.current_dir(cwd);
    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    if let Some(extra) = extra_path {
        let old = std::env::var_os("PATH").unwrap_or_default();
        let new = std::env::join_paths(
            std::env::split_paths(&old).chain(std::iter::once(extra.to_path_buf())),
        )
        .expect("PATH");
        cmd.env("PATH", new);
    }

    let command = argv.join(" ");
    match cmd.output() {
        Ok(output) => StepResult {
            command,
            code: output.status.code(),
            signal: output.status.signal(),
            stdout: output.stdout,
            stderr: output.stderr,
            duration: start.elapsed(),
        },
        Err(e) => StepResult {
            command,
            code: None,
            signal: None,
            stdout: Vec::new(),
            stderr: format!("vir-virtue: could not run {}: {}\n", argv[0], e).into_bytes(),
            duration: start.elapsed(),
        },
    }
}

fn fail(steps: Vec<StepResult>, msg: String, filecheck_stderr: Option<String>) -> Outcome {
    Outcome {
        status: Status::Fail,
        failure: Some(msg),
        filecheck_stderr,
        steps,
        duration: Duration::from_secs(0),
    }
}

/// Expand a trailing-glob pattern (`build/*.sv`) against `cwd`.
/// Non-glob arguments are returned as-is.  Matches are sorted for
/// run-to-run reproducibility.
fn expand_glob(cwd: &Path, pattern: &str) -> Vec<String> {
    if !pattern.contains('*') {
        return vec![pattern.to_string()];
    }
    let (dir_part, base) = match pattern.rsplit_once('/') {
        Some((dir, base)) => (Some(dir), base),
        None => (None, pattern),
    };
    let dir_path = match dir_part {
        Some(dir) => cwd.join(dir),
        None => cwd.to_path_buf(),
    };
    let (prefix, suffix) = match base.split_once('*') {
        Some((pre, suf)) => (pre, suf),
        None => return vec![pattern.to_string()],
    };
    let mut matches: Vec<String> = Vec::new();
    if let Ok(entries) = std::fs::read_dir(&dir_path) {
        for entry in entries.filter_map(|e| e.ok()) {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if name.starts_with(prefix) && name.ends_with(suffix) {
                match dir_part {
                    Some(dir) => matches.push(format!("{dir}/{name}")),
                    None => matches.push(name.to_string()),
                }
            }
        }
    }
    matches.sort();
    matches
}

// --- kind = "check" ------------------------------------------------------------

fn run_check(env: &Env, test: &Test) -> Outcome {
    let dir = env.test_dir(test);
    let argv = vec![env.vir.to_string_lossy().to_string(), test.check_cfg.command.clone()];
    let step = run_command(&argv, &dir, Some(&env.root.join("target/release")));
    let steps = vec![step];
    let merged = steps[0].merged();

    let contract_err = |why: String| fail(steps.clone(), why, None);

    match test.check_cfg.expect {
        Expect::Ok => {
            if steps[0].code != Some(0) {
                return contract_err(format!(
                    "vir {} failed ({})",
                    test.check_cfg.command,
                    steps[0].exit_description()
                ));
            }
        }
        Expect::OkNoWarnings => {
            if steps[0].code != Some(0) {
                return contract_err(format!(
                    "vir check failed ({})",
                    steps[0].exit_description()
                ));
            }
            if contains_word(&merged, b"WARNING") {
                return contract_err("unexpected warnings".to_string());
            }
        }
        Expect::OkWithWarnings => {
            if steps[0].code != Some(0) {
                return contract_err(format!(
                    "vir check reported errors ({})",
                    steps[0].exit_description()
                ));
            }
            if !contains_word(&merged, b"WARNING") {
                return contract_err("expected warnings, but none were emitted".to_string());
            }
        }
        Expect::Error => {
            if steps[0].code == Some(0) {
                return contract_err("expected vir check to fail, but it exited 0".to_string());
            }
        }
    }

    // filecheck: consumes stdout ++ stderr concatenated, byte-compatible
    // with today's `> out 2>&1`.
    if let Some(checkfile) = &test.check_cfg.filecheck {
        let check_path = dir.join(checkfile);
        let content = match std::fs::read_to_string(&check_path) {
            Ok(content) => content,
            Err(e) => {
                return fail(
                    steps,
                    format!("cannot read check file {}: {}", check_path.display(), e),
                    None,
                )
            }
        };
        let input = String::from_utf8_lossy(&merged);
        let input_lines: Vec<&str> = input.lines().collect();
        match crate::filecheck::run(&check_path, &content, &input_lines) {
            Ok(()) => {}
            Err(stderr) => {
                let first = stderr.lines().next().unwrap_or("filecheck failed");
                return fail(steps, first.to_string(), Some(stderr));
            }
        }
    }

    Outcome {
        status: Status::Pass,
        failure: None,
        filecheck_stderr: None,
        steps,
        duration: Duration::from_secs(0),
    }
}

/// `WARNING` detection, matching a `grep -q WARNING`:
/// a plain substring search over the merged output.
fn contains_word(haystack: &[u8], needle: &[u8]) -> bool {
    haystack.windows(needle.len()).any(|w| w == needle)
}

// --- kind = "sim" ---------------------------------------------------------------

fn run_sim(env: &Env, test: &Test) -> Outcome {
    let dir = env.test_dir(test);
    let target_release = env.root.join("target/release");
    let vir = env.vir.to_string_lossy().to_string();
    let mut steps: Vec<StepResult> = Vec::new();

    let build = run_command(&[vir.clone(), "build".to_string()], &dir, Some(&target_release));
    let build_ok = build.code == Some(0);
    let build_desc = build.exit_description();
    steps.push(build);
    if !build_ok {
        return fail(steps, format!("vir build failed ({build_desc})"), None);
    }

    let run = |argv: Vec<String>, steps: &mut Vec<StepResult>| {
        let result = run_command(&argv, &dir, Some(&target_release));
        let ok = result.code == Some(0);
        steps.push(result);
        ok
    };

    match test.sim_cfg.simulator {
        Simulator::Icarus => {
            let mut argv = vec!["iverilog".to_string()];
            argv.extend(test.sim_cfg.flags.iter().cloned());
            let sources = test.sim_cfg.sources.clone().unwrap_or_else(|| {
                vec!["build/*.sv".to_string(), "tb.sv".to_string()]
            });
            for source in &sources {
                argv.extend(expand_glob(&dir, source));
            }
            argv.extend(["-o".to_string(), "build/run".to_string()]);
            if !run(argv, &mut steps) {
                return fail(steps, "iverilog compile failed".to_string(), None);
            }
            if !run(vec!["./build/run".to_string()], &mut steps) {
                return fail(steps, "testbench run failed".to_string(), None);
            }
        }
        Simulator::Verilator => {
            let mut argv = vec!["verilator".to_string()];
            if test.sim_cfg.lint_only {
                argv.push("--lint-only".to_string());
            }
            argv.extend(test.sim_cfg.flags.iter().cloned());
            for source in expand_glob(&dir, "build/*.sv") {
                argv.push(source);
            }
            if !run(argv, &mut steps) {
                return fail(steps, "verilator failed".to_string(), None);
            }
        }
    }

    Outcome {
        status: Status::Pass,
        failure: None,
        filecheck_stderr: None,
        steps,
        duration: Duration::from_secs(0),
    }
}

// --- kind = "format" --------------------------------------------------------------

fn run_format(env: &Env, test: &Test) -> Outcome {
    let dir = env.test_dir(test);
    let target_release = env.root.join("target/release");
    let vir = env.vir.to_string_lossy().to_string();
    let scratch = dir.join("build").join("format");
    if std::fs::create_dir_all(&scratch).is_err() {
        return fail(vec![], format!("cannot create scratch dir {}", scratch.display()), None);
    }
    let input_src = dir.join(&test.format_cfg.input);
    let workfile = scratch.join("input.vir");
    if let Err(e) = std::fs::copy(&input_src, &workfile) {
        return fail(vec![], format!("cannot copy {}: {}", input_src.display(), e), None);
    }

    let mut steps: Vec<StepResult> = Vec::new();

    // `vir format` rewrites the file in place; its own exit code is not
    // the contract (same as `> /dev/null 2>&1;`).
    let fmt = run_command(
        &[vir.clone(), "format".to_string(), workfile.to_string_lossy().to_string()],
        &dir,
        Some(&target_release),
    );
    steps.push(fmt);

    // Golden comparison.
    let golden_path = dir.join(&test.format_cfg.golden);
    let formatted = std::fs::read(&workfile).unwrap_or_default();
    let golden = std::fs::read(&golden_path).unwrap_or_default();
    if formatted != golden {
        let excerpt = first_diff_excerpt(&golden, &formatted);
        return fail(
            steps,
            format!("formatted output differs from {}", test.format_cfg.golden.display()),
            Some(excerpt),
        );
    }

    // Parse gate: the formatted file must not have syntax errors.  The
    // runner greps for 'Syntax Error' instead of trusting the exit code.
    let parse = run_command(
        &[vir, "parse".to_string(), workfile.to_string_lossy().to_string()],
        &dir,
        Some(&target_release),
    );
    let parse_merged = parse.merged();
    steps.push(parse);
    if contains_word(&parse_merged, b"Syntax Error") {
        return fail(steps, "formatted file does not parse".to_string(), None);
    }

    Outcome {
        status: Status::Pass,
        failure: None,
        filecheck_stderr: None,
        steps,
        duration: Duration::from_secs(0),
    }
}

// --- kind = "golden" ----------------------------------------------------------------

fn run_golden(env: &Env, test: &Test) -> Outcome {
    let dir = env.test_dir(test);
    let target_release = env.root.join("target/release");
    let vir = env.vir.to_string_lossy().to_string();
    let mut steps: Vec<StepResult> = Vec::new();

    for command in ["check", "build"] {
        let step = run_command(&[vir.clone(), command.to_string()], &dir, Some(&target_release));
        let ok = step.code == Some(0);
        steps.push(step);
        if !ok {
            return fail(steps, format!("vir {command} failed"), None);
        }
    }

    for grep in &test.golden_cfg.grep {
        let path = dir.join(&grep.path);
        match std::fs::read(&path) {
            Ok(content) => {
                if !contains_word(&content, grep.pattern.as_bytes()) {
                    return fail(
                        steps,
                        format!("{} does not contain {:?}", grep.path.display(), grep.pattern),
                        None,
                    );
                }
            }
            Err(e) => {
                return fail(steps, format!("{}: {}", path.display(), e), None);
            }
        }
    }

    for diff in &test.golden_cfg.diff {
        let a = std::fs::read(dir.join(&diff.a)).unwrap_or_default();
        let b = std::fs::read(dir.join(&diff.b)).unwrap_or_default();
        let a_body: Vec<u8> = skip_lines_bytes(&a, diff.skip_lines);
        let b_body: Vec<u8> = skip_lines_bytes(&b, diff.skip_lines);
        if a_body != b_body {
            let excerpt = first_diff_excerpt(&a_body, &b_body);
            return fail(
                steps,
                format!("{} differs from {}", diff.a.display(), diff.b.display()),
                Some(excerpt),
            );
        }
    }

    Outcome {
        status: Status::Pass,
        failure: None,
        filecheck_stderr: None,
        steps,
        duration: Duration::from_secs(0),
    }
}

// --- kind = "doc" ---------------------------------------------------------------------

fn run_doc(env: &Env, test: &Test) -> Outcome {
    let dir = env.test_dir(test);
    let target_release = env.root.join("target/release");
    let vir = env.vir.to_string_lossy().to_string();
    let mut steps: Vec<StepResult> = Vec::new();

    let step = run_command(&[vir, "doc".to_string()], &dir, Some(&target_release));
    let ok = step.code == Some(0);
    steps.push(step);
    if !ok {
        return fail(steps, "vir doc failed".to_string(), None);
    }

    for file in &test.doc_cfg.expect_files {
        let path = dir.join(file);
        if !path.is_file() {
            return fail(steps, format!("expected file {} was not generated", file.display()), None);
        }
    }

    for grep in &test.doc_cfg.grep {
        let path = dir.join(&grep.path);
        match std::fs::read(&path) {
            Ok(content) => {
                if !contains_word(&content, grep.pattern.as_bytes()) {
                    return fail(
                        steps,
                        format!("{} does not contain {:?}", grep.path.display(), grep.pattern),
                        None,
                    );
                }
            }
            Err(e) => return fail(steps, format!("{}: {}", path.display(), e), None),
        }
    }

    Outcome {
        status: Status::Pass,
        failure: None,
        filecheck_stderr: None,
        steps,
        duration: Duration::from_secs(0),
    }
}

// --- helpers ----------------------------------------------------------------------------

fn skip_lines_bytes(data: &[u8], skip: usize) -> Vec<u8> {
    let text = String::from_utf8_lossy(data);
    let body: Vec<&str> = text.lines().skip(skip).collect();
    body.join("\n").into_bytes()
}

/// Small context excerpt around the first differing line, for the log.
fn first_diff_excerpt(expected: &[u8], actual: &[u8]) -> String {
    let expected = String::from_utf8_lossy(expected);
    let actual = String::from_utf8_lossy(actual);
    let e_lines: Vec<&str> = expected.lines().collect();
    let a_lines: Vec<&str> = actual.lines().collect();
    let mut excerpt = String::new();
    let first_diff = e_lines
        .iter()
        .zip(a_lines.iter())
        .position(|(e, a)| e != a)
        .unwrap_or_else(|| e_lines.len().min(a_lines.len()));
    let start = first_diff.saturating_sub(2);
    let end = first_diff + 3;
    for (i, line) in e_lines[start..end.min(e_lines.len())].iter().enumerate() {
        let idx = start + i;
        let marker = if idx == first_diff { "expected" } else { "        " };
        excerpt.push_str(&format!("{marker} -{idx}- {line}\n"));
    }
    for (i, line) in a_lines[start..end.min(a_lines.len())].iter().enumerate() {
        let idx = start + i;
        let marker = if idx == first_diff { "actual  " } else { "        " };
        excerpt.push_str(&format!("{marker} +{idx}- {line}\n"));
    }
    excerpt
}

/// Render the evidence log (`build/tests/<path>/test.log`).
pub fn render_log(env: &Env, test: &Test, outcome: &Outcome) -> String {
    let mut log = String::new();
    log.push_str(&format!("test: {}\n", test.name));
    log.push_str(&format!("dir: {}\n", env.test_dir(test).display()));
    log.push_str(&format!("status: {}\n", outcome.status.as_str()));
    log.push_str(&format!("duration: {:.3}s\n", outcome.duration.as_secs_f64()));
    if let Some(failure) = &outcome.failure {
        log.push_str(&format!("failure: {failure}\n"));
    }
    for (i, step) in outcome.steps.iter().enumerate() {
        log.push_str(&format!(
            "\n--- step {} ({}): {} ---\n",
            i + 1,
            step.exit_description(),
            step.command
        ));
        log.push_str(&format!("duration: {:.3}s\n", step.duration.as_secs_f64()));
        if !step.stdout.is_empty() {
            log.push_str("--- stdout ---\n");
            log.push_str(&String::from_utf8_lossy(&step.stdout));
            if !step.stdout.ends_with(b"\n") {
                log.push('\n');
            }
        }
        if !step.stderr.is_empty() {
            log.push_str("--- stderr ---\n");
            log.push_str(&String::from_utf8_lossy(&step.stderr));
            if !step.stderr.ends_with(b"\n") {
                log.push('\n');
            }
        }
        log.push_str("--- merged (stdout 2>&1) ---\n");
        let merged = step.merged();
        log.push_str(&String::from_utf8_lossy(&merged));
        if !merged.ends_with(b"\n") {
            log.push('\n');
        }
    }
    if let Some(stderr) = &outcome.filecheck_stderr {
        log.push_str("\n--- filecheck stderr ---\n");
        log.push_str(stderr);
        if !stderr.ends_with('\n') {
            log.push('\n');
        }
    }
    log
}

/// Write `build/tests/<path>/test.log`.  Written iff the test failed.
pub fn write_log(env: &Env, test: &Test, outcome: &Outcome) -> Option<PathBuf> {
    if outcome.status != Status::Fail {
        return None;
    }
    let dir = env.log_dir(test);
    if std::fs::create_dir_all(&dir).is_err() {
        return None;
    }
    let path = dir.join("test.log");
    match std::fs::File::create(&path).and_then(|mut f| f.write_all(render_log(env, test, outcome).as_bytes())) {
        Ok(()) => Some(path),
        Err(_) => None,
    }
}
