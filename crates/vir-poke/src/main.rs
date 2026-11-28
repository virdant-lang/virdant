//! The `vir-poke` binary: a REPL-driven peek/poke tester built on the
//! `sim` library, with a small custom scripting language whose
//! expressions use Virdant syntax.  Scripts double as assertion tests.
//!
//! `vir poke` execs this binary via the `vir-*` companion-binary
//! mechanism; see `vir/src/vir/cli.rs`.
//!
//! A script passed bare (or via `--script`) runs standalone as an
//! assertion test; `--repl` instead drops into the interactive REPL
//! on the first failure.
//!
//! This binary owns only the command-line interface, project
//! discovery, and script-file reading; the session and the
//! script/REPL engine live in the WASM-clean `vir_poke` library
//! (`lib.rs`/`driver.rs`), which is also what a WASM front-end
//! (WASI/wasmtime) links.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use bstr::ByteSlice;
use clap::value_parser;

// Shared helpers, compiled once as the `vir_common` library.
use vir_common as common;

use common::ProjectLocation;

use virdant::analysis::symbols::SymbolKind;
use virdant::sim::{InitStrategy, Sim};

use vir_poke::{driver, Session};

fn command() -> clap::Command {
    let mut cmd = clap::Command::new("vir-poke")
        .about("Interactive peek/poke tester")
        .arg(clap::Arg::new("top").long("top").num_args(1)
            .value_parser(value_parser!(String))
            .help("Top module (may be non-pub); defaults to [prog] top"))
        .arg(clap::Arg::new("file").value_name("SCRIPT").num_args(0..=1)
            .value_parser(value_parser!(String))
            .help("Script to run standalone as an assertion test"))
        .arg(clap::Arg::new("repl").long("repl").short('r')
            .num_args(0)
            .requires("file")
            .conflicts_with("script")
            .help("Run SCRIPT, dropping into the REPL on the first failure"))
        .arg(clap::Arg::new("script").long("script").value_name("SCRIPT").num_args(0..=1)
            .default_missing_value("")
            .value_parser(value_parser!(String))
            .help("Run a script standalone as an assertion test (same as passing SCRIPT bare)"))
        // Hidden: a machine-readable exit hook.  Requires a positional
        // SCRIPT, which is parsed and scanned but *not run*: each
        // `show <path>` in it names a signal for the summary (in
        // script order) and each `format` sets that signal's display
        // radix.  A JSON array — one object per `show` — prints to
        // stdout; nothing else is written there.
        .arg(clap::Arg::new("format_json").long("format-json")
            .num_args(0)
            .requires("file")
            .conflicts_with("script")
            .conflicts_with("repl")
            .hide(true));
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);
    let top = matches.get_one::<String>("top").cloned();
    let file = matches.get_one::<String>("file").cloned();
    let script = matches.get_one::<String>("script").cloned();
    let repl_flag = matches.get_flag("repl");
    let format_json = matches.get_flag("format_json");

    // Forms:
    //   vir poke                  — REPL
    //   vir poke test.poke        — standalone script run (assertion test)
    //   vir poke test.poke --script / vir poke --script test.poke
    //                             — standalone script run (same as bare)
    //   vir poke test.poke --repl — run the script, dropping into the
    //                               REPL on the first failure
    //   vir poke --format-json test.poke — scan the script, print a
    //                               JSON signal summary (never run)
    let (preload, standalone) = if repl_flag {
        // clap enforces `--repl` requires a file and conflicts with
        // `--script`, so `file` is always present here.
        match file {
            Some(file) => (Some(file), None),
            None => {
                eprintln!("error: --repl needs a script file");
                std::process::exit(2);
            }
        }
    } else {
        match (file, script.as_deref()) {
            // `vir poke test.poke --script`: bare --script flag.
            (Some(file), Some("")) => (None, Some(file)),
            // `vir poke a.poke --script b.poke`: conflicting.
            (Some(_), Some(_)) => {
                eprintln!("error: cannot give both a script and --script <path>");
                std::process::exit(2);
            }
            // `vir poke test.poke`: standalone run, like `--script`.
            (Some(file), None) => (None, Some(file)),
            (None, Some("")) => {
                eprintln!("error: --script needs a script file");
                std::process::exit(2);
            }
            (None, Some(path)) => (None, Some(path.to_string())),
            (None, None) => (None, None),
        }
    };

    let code = run(&location, top, preload, standalone, format_json);
    std::process::exit(code);
}

/// Run `vir poke`.
///
/// `top_arg` is the explicit `--top` (may be non-pub); when absent, the
/// top comes from `[prog] top` in `virdant.toml`.  `standalone` is a
/// script run as an assertion test; `preload` (from `--repl`) a script
/// run with the REPL taking over on the first failure.  Returns the
/// process exit code.
pub fn run(
    location: &vir_common::ProjectLocation,
    top_arg: Option<String>,
    preload: Option<String>,
    standalone: Option<String>,
    format_json: bool,
) -> i32 {
    if preload.is_some() && standalone.is_some() {
        eprintln!("error: cannot both preload and run a script standalone");
        return 2;
    }

    let cwd = location.resolve_cwd();
    let db = location.project_db();

    // Resolve the top module: explicit `--top` first, else `[prog] top`.
    let allow_non_pub = top_arg.is_some();
    let top_name = match top_arg {
        Some(name) => name,
        None => match common::read_prog_key(&cwd, "top") {
            Some(name) => name,
            None => {
                eprintln!("error: no top module: pass --top or set [prog] top in virdant.toml");
                return 2;
            }
        },
    };

    let symboltable = db.get_symboltable();
    let exact = symboltable
        .items()
        .into_iter()
        .find(|sym| sym.kind == SymbolKind::ModDef && sym.fqn.as_bytes() == top_name.as_bytes());
    let top_symbol = match exact {
        Some(sym) => sym,
        None => {
            // Fall back to a simple-name match (the top named without
            // its package qualifier).  Ambiguity is an error.
            let matches: Vec<_> = symboltable
                .items()
                .into_iter()
                .filter(|sym| sym.kind == SymbolKind::ModDef && sym.name().as_bytes() == top_name.as_bytes())
                .collect();
            match matches.len() {
                0 => {
                    eprintln!("error: top module '{top_name}' not found");
                    return 2;
                }
                1 => matches[0].clone(),
                _ => {
                    let names: Vec<String> = matches
                        .iter()
                        .map(|sym| sym.fqn().to_str_lossy().into_owned())
                        .collect();
                    eprintln!(
                        "error: top module '{top_name}' is ambiguous: {}",
                        names.join(", ")
                    );
                    return 2;
                }
            }
        }
    };

    // Check the project.  An explicit `--top` may name a non-pub
    // module: the tester bypasses the pub-check by dropping the
    // `TopNotPub` diagnostics (a non-pub mod with a `for` clause) —
    // they gate top selection, which is exactly what `--top` overrides.
    let top_fqn = top_symbol.fqn().to_str_lossy().into_owned();
    let diagnostics = match virdant::util::check_db(&db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };
    let diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|diag| {
            if allow_non_pub
                && matches!(&diag.payload, virdant::diagnostics::DiagnosticPayload::TopNotPub(_))
            {
                return false;
            }
            true
        })
        .collect();
    let mut failed = false;
    for diagnostic in &diagnostics {
        common::dump_diagnostic(&db, diagnostic);
        if diagnostic.level() == virdant::diagnostics::DiagnosticLevel::Error {
            failed = true;
        }
    }
    if failed {
        eprintln!("Check failed");
        return 1;
    }

    let sim = Sim::new(&db, top_symbol.id(), InitStrategy::Random, &Default::default());
    let mut session = Session {
        db,
        sim,
        top: top_symbol.id(),
        cwd: Vec::new(),
        it: None,
        break_flag: Rc::new(Cell::new(None)),
        autoshow: false,
        formats: Rc::new(RefCell::new(HashMap::new())),
        vcd: None,
        vcd_signals: Rc::new(RefCell::new(None)),
        // The running script's name: the default `vcd` file name is
        // derived from it (`<script>.vcd`).
        script_name: standalone
            .clone()
            .or_else(|| preload.clone()),
        format_json_signals: RefCell::new(Vec::new()),
    };

    // `--format-json`: the script is required (clap enforces it) and
    // is parsed and scanned — never run: its `show` commands name the
    // signals in the summary, its `format` commands set their display
    // radices.  The summary itself is the only stdout output.
    if format_json {
        let path = standalone
            .or(preload)
            .expect("--format-json requires a script file");
        let lines = match read_script(&path) {
            Ok(lines) => lines,
            Err(code) => return code,
        };
        if let Err(e) = session.scan_format_json(&lines) {
            eprintln!("error: {path}: {e}");
            return 1;
        }
        print!("{}", session.format_json());
        return 0;
    }

    let script = if let Some(path) = standalone {
        match read_script(&path) {
            Ok(lines) => Some((lines, true)),
            Err(code) => return code,
        }
    } else if let Some(path) = preload {
        match read_script(&path) {
            Ok(lines) => Some((lines, false)),
            Err(code) => return code,
        }
    } else {
        None
    };

    if script
        .as_ref()
        .map(|(_, standalone)| !*standalone)
        .unwrap_or(true)
    {
        println!("vir poke: top {top_fqn} (type `exit` to leave, `help` for commands)");
    }
    driver::run(session, script)
}

/// Read a `.poke` script file.
fn read_script(path: &str) -> Result<Vec<(usize, String)>, i32> {
    match std::fs::read_to_string(path) {
        Ok(text) => Ok(text
            .lines()
            .enumerate()
            .map(|(i, line)| (i + 1, line.to_string()))
            .collect()),
        Err(e) => {
            eprintln!("error: cannot read script {path}: {e}");
            Err(2)
        }
    }
}
