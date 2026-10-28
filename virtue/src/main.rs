//! The Virdant test runner (`virtue`).
//!
//! A pure orchestrator: it links nothing from the compiler.  Every test
//! is a subprocess of `target/release/vir` (plus iverilog / verilator
//! for the sim suite), run with the same commands and exit-code
//! contracts the per-directory Makefiles used before the runner replaced
//! them.
//!
//! Usage:
//!   virtue                     run all tests
//!   virtue <test> ...          run only the named tests
//!   virtue --list              list all tests
//!   virtue --suite sim         run one suite
//!   virtue --filter substr     run matching tests
//!   virtue --filecheck <file>  standalone FileCheck mode

use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::mpsc;
use std::sync::Arc;
use std::time::{Duration, Instant};

use clap::Parser;

use virtue::discovery;
use virtue::exec;
use virtue::filecheck;
use virtue::output;

use discovery::Test;
use exec::{Env, Status};

/// Worker → main thread events: a test was claimed (about to run) or
/// finished.
enum WorkerMsg {
    Started(usize),
    Done(usize, Status, Option<String>),
}

/// Set by the SIGINT handler (TTY mode only); the main loop performs
/// the terminal cleanup and exits.
static INTERRUPTED: AtomicBool = AtomicBool::new(false);

extern "C" fn on_sigint(_: libc::c_int) {
    INTERRUPTED.store(true, std::sync::atomic::Ordering::SeqCst);
}

/// The Virdant test runner
#[derive(Parser, Debug)]
#[command(name = "virtue", disable_help_subcommand = true)]
struct Args {
    /// Standalone FileCheck mode: match CHECK directives in <check-file> against stdin
    #[arg(long)]
    filecheck: bool,

    /// Run only one suite (top-level group of the test path, e.g. `sim`)
    #[arg(long)]
    suite: Option<String>,

    /// Run only tests whose name contains <substr>
    #[arg(long)]
    filter: Option<String>,

    /// Number of worker threads (default: min(cores, 16))
    #[arg(short = 'j', long)]
    jobs: Option<usize>,

    /// List all tests and exit
    #[arg(long)]
    list: bool,

    /// Stop on the first failure (drain in-flight tests, print test.log)
    #[arg(long, alias = "ff")]
    fail_fast: bool,

    /// Tests to run (stable names, e.g. tests/errors/typing/word_literals)
    tests: Vec<String>,
}

fn main() -> ExitCode {
    // Die quietly on `virtue --list | head` instead of panicking on
    // a broken stdout pipe.
    unsafe { libc::signal(libc::SIGPIPE, libc::SIG_DFL) };

    // `-ff` is spelled as one flag; map it onto --fail-fast for clap.
    let raw: Vec<String> = std::env::args()
        .map(|arg| if arg == "-ff" { "--fail-fast".to_string() } else { arg })
        .collect();
    let args = Args::parse_from(raw.clone());

    if args.filecheck {
        let rest: Vec<String> = raw.iter().skip(1).filter(|a| a.as_str() != "--fail-fast").cloned().collect();
        let check_file = rest.into_iter().find(|a| a != "--filecheck");
        filecheck::cli_main(check_file);
    }

    match run_all(args) {
        Ok(()) => ExitCode::SUCCESS,
        Err(code) => code.into(),
    }
}

enum RunErr {
    /// Loud config/policy error, before any test runs.
    Fatal(String),
    /// Tests ran; some failed.
    Failed,
}

impl From<RunErr> for ExitCode {
    fn from(e: RunErr) -> ExitCode {
        match e {
            RunErr::Fatal(msg) => {
                eprintln!("virtue: {msg}");
                ExitCode::from(2)
            }
            RunErr::Failed => ExitCode::from(1),
        }
    }
}

fn find_root() -> Result<PathBuf, RunErr> {
    let mut dir = std::env::current_dir().map_err(|e| RunErr::Fatal(format!("cannot get cwd: {e}")))?;
    loop {
        if dir.join("tests").is_dir() && dir.join("target").is_dir() {
            return Ok(dir);
        }
        match dir.parent() {
            Some(parent) => dir = parent.to_path_buf(),
            None => {
                return Err(RunErr::Fatal(
                    "could not find the repository root (no tests/ + target/ above the cwd)".to_string(),
                ))
            }
        }
    }
}

fn run_all(args: Args) -> Result<(), RunErr> {
    let root = find_root()?;
    let vir = root.join("target/release/vir");
    if !vir.is_file() {
        return Err(RunErr::Fatal(format!(
            "{} is missing; run `make build` first",
            vir.display()
        )));
    }
    let env = Env { root: root.clone(), vir };

    let tests = discovery::discover(&root).map_err(RunErr::Fatal)?;

    // Selection: --suite, --filter, and positional names.
    let selected: Vec<usize> = {
        let mut sel: Vec<usize> = (0..tests.len()).collect();
        if let Some(suite) = &args.suite {
            sel.retain(|&i| suite_of(&tests[i]) == Some(suite.as_str()));
            if sel.is_empty() {
                return Err(RunErr::Fatal(format!("no tests in suite `{suite}`")));
            }
        }
        if let Some(filter) = &args.filter {
            sel.retain(|&i| tests[i].name.contains(filter.as_str()));
        }
        if !args.tests.is_empty() {
            let by_name: std::collections::HashMap<&str, usize> =
                tests.iter().enumerate().map(|(i, t)| (t.name.as_str(), i)).collect();
            let mut named = Vec::new();
            for name in &args.tests {
                match by_name.get(name.as_str()) {
                    Some(&i) => named.push(i),
                    None => {
                        return Err(RunErr::Fatal(format!(
                            "unknown test `{name}` (see `virtue --list`)"
                        )))
                    }
                }
            }
            named.sort_unstable();
            named.dedup();
            sel = named;
        }
        sel
    };

    if args.list {
        for &i in &selected {
            println!("{}", tests[i].name);
        }
        return Ok(());
    }

    if selected.is_empty() {
        return Err(RunErr::Fatal("no tests matched".to_string()));
    }

    // Simulator policy: the sim suite runs every project when the
    // simulators are installed; otherwise it fails loudly unless
    // VIRDANT_ALLOW_MISSING_SIM=1 is set, in which case every such test
    // is skipped with a notice instead.
    let mut needed: Vec<&'static str> = Vec::new();
    for &i in &selected {
        if let Some(binary) = tests[i].needs_simulator() {
            if !needed.contains(&binary) {
                needed.push(binary);
            }
        }
    }
    let allow_missing_sim =
        std::env::var("VIRDANT_ALLOW_MISSING_SIM").ok().as_deref() == Some("1");
    let missing: Vec<&'static str> =
        needed.into_iter().filter(|bin| !find_on_path(bin)).collect();
    if !missing.is_empty() && !allow_missing_sim {
        return Err(RunErr::Fatal(format!(
            "MISSING SIMULATOR TOOLS: {} (install them, or set VIRDANT_ALLOW_MISSING_SIM=1 to skip sim tests)",
            missing.join(" ")
        )));
    }
    let mut selected_tests: Vec<Test> = Vec::new();
    for &i in &selected {
        let mut test = tests[i].clone();
        if test.needs_simulator().map(|bin| missing.contains(&bin)).unwrap_or(false) {
            test.skip = Some(format!(
                "simulator tools missing ({})",
                test.needs_simulator().unwrap_or("")
            ));
        }
        selected_tests.push(test);
    }

    let jobs = args
        .jobs
        .unwrap_or_else(|| {
            std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1).min(16)
        })
        .clamp(1, 1024);

    run_suite(&env, &selected_tests, jobs, args.fail_fast).map_err(|e| e)
}

fn suite_of(test: &Test) -> Option<&str> {
    // The suite is the test path's top-level group: for the conventional
    // tests/ tree that's the verification layer (check, errors, sim, ...);
    // elsewhere it's the first directory component.
    let name = test.name.strip_prefix("tests/").unwrap_or(&test.name);
    name.split('/').next()
}

fn find_on_path(binary: &str) -> bool {
    if let Some(paths) = std::env::var_os("PATH") {
        for dir in std::env::split_paths(&paths) {
            if dir.join(binary).is_file() {
                return true;
            }
        }
    }
    false
}

/// Dispatch in canonical order, thread-per-test, and render output.
fn run_suite(env: &Env, tests: &[Test], jobs: usize, fail_fast: bool) -> Result<(), RunErr> {
    let start = Instant::now();
    let total = tests.len();
    let renderer = output::Renderer::new(tests, output::term_size());
    let spinner_base = Instant::now();

    // Ctrl+C handling applies to the screen display only: the handler
    // just raises a flag (async-signal-safe); the main loop performs
    // the terminal cleanup and exits.  In piped mode the default
    // SIGINT disposition (immediate termination) is kept.
    if renderer.is_screen() {
        unsafe { libc::signal(libc::SIGINT, on_sigint as extern "C" fn(libc::c_int) as libc::sighandler_t) };
    }

    let next = Arc::new(AtomicUsize::new(0));
    let stop = Arc::new(AtomicBool::new(false));
    let (tx, rx) = mpsc::channel::<WorkerMsg>();
    let env_arc = Arc::new(Env { root: env.root.clone(), vir: env.vir.clone() });
    let tests_arc = Arc::new(tests.to_vec());

    let mut handles = Vec::new();
    for _ in 0..jobs {
        let stop = stop.clone();
        let tx = tx.clone();
        let env = env_arc.clone();
        let tests = tests_arc.clone();
        let next = next.clone();
        handles.push(std::thread::spawn(move || loop {
            if stop.load(Ordering::SeqCst) {
                break;
            }
            let idx = next.fetch_add(1, Ordering::SeqCst);
            if idx >= tests.len() {
                break;
            }
            let test = &tests[idx];
            tx.send(WorkerMsg::Started(idx)).ok();
            let outcome = exec::run_test(&env, test);
            let log_path = exec::write_log(&env, test, &outcome)
                .map(|p| {
                    // Report the log relative to the repo root.
                    p.strip_prefix(&env.root)
                        .unwrap_or(&p)
                        .display()
                        .to_string()
                });
            tx.send(WorkerMsg::Done(idx, outcome.status, log_path)).ok();
        }));
    }
    drop(tx);

    let mut statuses: Vec<Option<Status>> = vec![None; total];
    let mut log_paths: Vec<Option<String>> = vec![None; total];
    let mut running: std::collections::BTreeSet<usize> = Default::default();
    // The dispatch frontier: the highest test index handed to a worker.
    let mut frontier = 0usize;

    loop {
        const TICK: Duration = Duration::from_millis(120);
        match rx.recv_timeout(TICK) {
            Ok(WorkerMsg::Started(idx)) => {
                running.insert(idx);
                frontier = frontier.max(idx);
            }
            Ok(WorkerMsg::Done(idx, status, log_path)) => {
                running.remove(&idx);
                statuses[idx] = Some(status);
                log_paths[idx] = log_path;
                frontier = frontier.max(idx);
                if !renderer.is_screen() {
                    // Piped mode: plain output, one line per test,
                    // printed only once that test has finished; no
                    // in-progress lines, no escape codes.
                    println!("{}", renderer.line(&tests[idx], Some(status), 0));
                }
                if fail_fast && status == Status::Fail {
                    stop.store(true, Ordering::SeqCst);
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {}
            Err(mpsc::RecvTimeoutError::Disconnected) => break,
        }

        if INTERRUPTED.load(Ordering::SeqCst) {
            // Clear the cursor's line (which holds the progress line
            // and the terminal's ^C echo) and everything below it, then
            // move to the next line, first column — the shell prompt
            // will land there, with the test lines above kept on
            // screen.
            let mut out = std::io::stdout().lock();
            write!(out, "\r\x1b[2K\x1b[0J\n\r").ok();
            out.flush().ok();
            std::process::exit(130); // 128 + SIGINT
        }

        let spinner = (spinner_base.elapsed().as_millis() / 120) as usize;
        // On a terminal: repaint the alternate-screen board in place
        // (the main screen and its scrollback are untouched).
        // Piped output stays silent during the run so that the per-test
        // lines always appear in canonical order; printed after the loop.
        renderer.frame(tests, &statuses, frontier, spinner);
    }

    for handle in handles {
        handle.join().ok();
    }

    let wall = start.elapsed();
    if renderer.is_screen() {
        // Summary into the reserved viewport rows: no clearing, no
        // scrolling, the board stays on screen.
        renderer.end_summary(tests, &statuses, wall);
    } else {
        // (Test lines were already printed as each test finished.)
        output::summary(tests, &statuses, &log_paths, wall);
    }

    let failed_at: Option<usize> = statuses
        .iter()
        .position(|s| matches!(s, Some(Status::Fail)));
    if let Some(idx) = failed_at {
        if fail_fast {
            // Print the canonical-earliest failure's test.log to stderr.
            if let Some(path) = &log_paths[idx] {
                let full = env.root.join(path);
                if let Ok(log) = std::fs::read_to_string(&full) {
                    eprintln!("\n{}", log);
                }
            }
        }
        return Err(RunErr::Failed);
    }
    Ok(())
}
