//! simtrace — differential-trace harness runner for the simulator.
//!
//! Builds the `Db` for a project, constructs a `Sim` with
//! `InitStrategy::Seeded(S)`, runs N clock cycles, and writes one line
//! per observed (cycle, signal) pair:
//!
//!     <cycle> <path> <hex> <bin>
//!
//! `<hex>` is the value's bits as zero-padded lowercase hex (padded on
//! the left to a whole number of nibbles); `<bin>` is the full
//! MSB-first bit string, so the pair is unambiguous for any width.
//! Zero-width signals are skipped (the same convention as the VCD
//! dumper).  Signals are emitted in path order.
//!
//! Dump point: the initial, fully-settled state is written as cycle 0
//! (after `Sim::new`'s initial propagation and `attach_clock`'s
//! initial low write, before any clock edge).  Each subsequent
//! snapshot is taken inside an `on_clock` rising-edge watcher on the
//! attached clock: at that point the edge has transferred every
//! register and `flow` has re-settled the combinational network
//! (watcher dispatch always runs after settlement), so the dump is a
//! complete, consistent post-edge state.  The harness registers no
//! other callbacks and drives no signals, so nothing perturbs the
//! circuit between settlement and the dump.
//!
//! Two runs with the same `--seed` and `--engine` over the same design
//! produce byte-identical output.  The trace itself is
//! engine-agnostic (no engine field anywhere in it), so a VM run and
//! a `--legacy` run over the same design and seed are directly
//! diffable — that is the contract `sim-bench/check.sh` and
//! `sim-bench/sweep.sh` are built on: the legacy engine is the
//! reference, and a VM trace must match it byte for byte.
//!
//! Designs with no `top.clock` (purely combinational) get only the
//! cycle-0 snapshot; the sweep uses that for clockless designs.

use std::cell::{Cell, RefCell};
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::rc::Rc;

use bstr::{BString, ByteSlice};
use virdant::analysis::elaboration::SignalId;
use virdant::analysis::symbols::SymbolKind;
use virdant::db::Db;
use virdant::sim::{Clock, Engine, InitStrategy, Sim};
use virdant::util::{check_db, db_from_dir_with_lib};

/// Resolve the top module.  `--top auto` (the default) picks the
/// module definition whose fully-qualified name ends in `::Top` (the
/// conventional design top: every test design names its top `Top`).
/// This mirrors what `vir run` accepts as a top (`[prog] top`) while
/// letting the differential sweep cover designs whose manifest carries
/// no `[prog] top` (the `kind = "sim"` tests, whose tops are exercised
/// by an external Verilog testbench).
fn resolve_top_auto(db: &Db) -> Option<virdant::analysis::symbols::SymbolId> {
    let symboltable = db.get_symboltable();
    let mut candidates: Vec<_> = symboltable.items().into_iter()
        .filter(|sym| sym.kind == SymbolKind::ModDef)
        .filter(|sym| sym.fqn.as_bytes().ends_with(b"::Top"))
        .collect();
    candidates.sort_by(|a, b| a.fqn.cmp(&b.fqn));
    match candidates.len() {
        0 => {
            eprintln!("simtrace: no `::Top` module found; pass --top PATH explicitly");
            None
        }
        1 => Some(candidates[0].id),
        _ => {
            eprintln!("simtrace: multiple `::Top` modules; pass --top PATH explicitly:");
            for sym in &candidates {
                eprintln!("  {}", sym.fqn.to_str_lossy());
            }
            None
        }
    }
}

struct Args {
    project: PathBuf,
    cycles: u64,
    seed: u64,
    out: PathBuf,
    top: Option<String>,
    engine: Engine,
}

const USAGE: &str = "\
usage: simtrace --project DIR [--cycles N] [--seed S] [--out FILE] [--top PATH] [--engine vm|legacy]
  --project DIR   project directory (containing src/ and virdant.toml)
  --cycles N      number of clock cycles to run (default 16)
  --seed S        seed for InitStrategy::Seeded (default 0)
  --out FILE      trace output file (default: stdout)
  --top PATH      top module symbol path (default top::Top)
  --engine WHICH  vm (default) or legacy";

fn parse_args() -> Args {
    let mut args = Args {
        project: PathBuf::from("."),
        cycles: 16,
        seed: 0,
        out: PathBuf::new(),
        top: None,
        engine: Engine::Vm,
    };
    let mut argv = std::env::args().skip(1);
    while let Some(arg) = argv.next() {
        let mut value_for = |flag: &str| {
            argv.next().unwrap_or_else(|| {
                eprintln!("simtrace: {flag} expects a value\n{USAGE}");
                std::process::exit(2);
            })
        };
        match arg.as_str() {
            "--project" => args.project = PathBuf::from(value_for("--project")),
            "--cycles" => args.cycles = value_for("--cycles").parse().unwrap_or_else(|_| {
                eprintln!("simtrace: --cycles expects a number\n{USAGE}");
                std::process::exit(2);
            }),
            "--seed" => args.seed = value_for("--seed").parse().unwrap_or_else(|_| {
                eprintln!("simtrace: --seed expects a number\n{USAGE}");
                std::process::exit(2);
            }),
            "--out" => args.out = PathBuf::from(value_for("--out")),
            "--top" => args.top = Some(value_for("--top")),
            "--engine" => {
                args.engine = match value_for("--engine").as_str() {
                    "vm" => Engine::Vm,
                    "legacy" => Engine::Legacy,
                    other => {
                        eprintln!("simtrace: unknown engine {other:?} (use vm | legacy)\n{USAGE}");
                        std::process::exit(2);
                    }
                }
            }
            _ => {
                eprintln!("simtrace: unknown argument {arg:?}\n{USAGE}");
                std::process::exit(2);
            }
        }
    }
    args
}

/// Every elaborated component, sorted by path.  Uses `Sim::components`
/// — the exhaustive flat list — since module instances are not
/// themselves components, so a `children_of` walk cannot reach
/// submodule-internal signals.
fn collect_signals(sim: &Sim) -> Vec<(SignalId, BString)> {
    let mut out = sim.components();
    out.sort_by(|a, b| a.1.cmp(&b.1));
    out
}

/// Zero-padded lowercase hex of an MSB-first bit string.
fn to_hex(bits: &str) -> String {
    let mut padded = bits.to_string();
    while padded.len() % 4 != 0 {
        padded.insert(0, '0');
    }
    let mut out = String::with_capacity(padded.len() / 4);
    for chunk in padded.as_bytes().chunks(4) {
        let mut nibble = 0u8;
        for (i, b) in chunk.iter().enumerate() {
            if *b == b'1' {
                nibble |= 1 << (3 - i);
            }
        }
        out.push(char::from_digit(nibble as u32, 16).unwrap());
    }
    out
}

type Writer = Rc<RefCell<BufWriter<Box<dyn Write>>>>;

fn write_snapshot(
    sim: &Sim,
    signals: &[(SignalId, BString)],
    cycle: u64,
    writer: &Writer,
) {
    let mut w = writer.borrow_mut();
    for (id, path) in signals {
        let value = sim.get(*id);
        let width = value.width();
        if width == 0 {
            continue;
        }
        let bits = value.to_bit_string();
        writeln!(w, "{cycle} {} {} {bits}", path.to_str_lossy(), to_hex(&bits))
            .expect("write trace line");
    }
}

fn main() {
    let args = parse_args();

    let source_dir = args.project.join("src");
    let db = db_from_dir_with_lib(source_dir, "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        eprintln!("simtrace: {} failed to build", args.project.display());
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let top_symbol = match &args.top {
        Some(top) => {
            let Some(sym) = symboltable.resolve(top.as_bytes().into()) else {
                eprintln!("simtrace: top symbol {:?} not found", top);
                std::process::exit(1);
            };
            sym.clone()
        }
        None => {
            let Some(sym_id) = resolve_top_auto(&db) else {
                std::process::exit(1);
            };
            symboltable.symbol(sym_id).clone()
        }
    };
    let top_name = top_symbol.fqn.to_str_lossy().to_string();

    let mut sim = Sim::with_engine(&db, top_symbol.id, InitStrategy::Seeded(args.seed), args.engine);

    // Designs with no clock on the top (purely combinational test
    // designs) have no edges to run: emit the settled cycle-0 snapshot
    // only.
    let clock_path = match top_name.rsplit_once("::") {
        Some((pkg, _)) => format!("{pkg}.clock"),
        None => "clock".to_string(),
    };
    let clock = sim.try_resolve(clock_path.as_bytes());

    let signals = collect_signals(&sim);

    let writer: Writer = if args.out.as_os_str().is_empty() {
        Rc::new(RefCell::new(BufWriter::new(Box::new(std::io::stdout().lock()))))
    } else {
        let file = std::fs::File::create(&args.out)
            .unwrap_or_else(|e| panic!("simtrace: cannot create {:?}: {e}", args.out));
        Rc::new(RefCell::new(BufWriter::new(Box::new(file))))
    };
    {
        let mut w = writer.borrow_mut();
        writeln!(w, "# virdant-trace v1").unwrap();
        writeln!(
            w,
            "# top={} seed={} cycles={} signals={}",
            top_name,
            args.seed,
            args.cycles,
            signals.len(),
        )
        .unwrap();
        writeln!(w, "# format: <cycle> <path> <hex> <bin>").unwrap();
    }

    let Some(clock) = clock else {
        write_snapshot(&sim, &signals, 0, &writer);
        writer.borrow_mut().flush().unwrap();
        return;
    };
    sim.attach_clock(clock, Clock::with_period_ps(10_000));

    write_snapshot(&sim, &signals, 0, &writer);

    let cycle_count: Rc<Cell<u64>> = Rc::new(Cell::new(0));
    let cycle_count_cb = cycle_count.clone();
    let writer_cb = writer.clone();
    let cycles = args.cycles;
    sim.on_clock(clock, Box::new(move |sim| {
        let cycle = cycle_count_cb.get() + 1;
        cycle_count_cb.set(cycle);
        write_snapshot(sim, &signals, cycle, &writer_cb);
        if cycle >= cycles {
            sim.finish();
        }
    }));

    sim.run().unwrap();
    writer.borrow_mut().flush().unwrap();
}
