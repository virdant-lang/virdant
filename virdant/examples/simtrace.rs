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
//! Two runs with the same `--seed` over the same design produce
//! byte-identical output; that is the contract the future simulator
//! rewrite is diffed against (see `sim-bench/README.md`).

use std::cell::{Cell, RefCell};
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::rc::Rc;

use bstr::{BString, ByteSlice};
use virdant::analysis::elaboration::SignalId;
use virdant::sim::{Clock, InitStrategy, Sim};
use virdant::util::{check_db, db_from_dir_with_lib};

struct Args {
    project: PathBuf,
    cycles: u64,
    seed: u64,
    out: PathBuf,
    top: String,
}

const USAGE: &str = "\
usage: simtrace --project DIR [--cycles N] [--seed S] [--out FILE] [--top PATH]
  --project DIR   project directory (containing src/ and virdant.toml)
  --cycles N      number of clock cycles to run (default 16)
  --seed S        seed for InitStrategy::Seeded (default 0)
  --out FILE      trace output file (default: stdout)
  --top PATH      top module symbol path (default top::Top)";

fn parse_args() -> Args {
    let mut args = Args {
        project: PathBuf::from("."),
        cycles: 16,
        seed: 0,
        out: PathBuf::new(),
        top: "top::Top".into(),
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
            "--top" => args.top = value_for("--top"),
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
    let Some(top) = symboltable.resolve(args.top.as_bytes().into()) else {
        eprintln!("simtrace: top symbol {:?} not found", args.top);
        std::process::exit(1);
    };

    let mut sim = Sim::new(&db, top.id(), InitStrategy::Seeded(args.seed));

    let Some(clock) = sim.try_resolve("top.clock") else {
        eprintln!("simtrace: no `top.clock` signal to attach a clock to");
        std::process::exit(1);
    };
    sim.attach_clock(clock, Clock::with_period_ps(10_000));

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
            args.top,
            args.seed,
            args.cycles,
            signals.len(),
        )
        .unwrap();
        writeln!(w, "# format: <cycle> <path> <hex> <bin>").unwrap();
    }

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
