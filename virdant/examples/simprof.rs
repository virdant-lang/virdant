//! simprof: in-process phase attribution for the simulator run loop.
//!
//! Builds the `Db` for a project, constructs a `Sim` with
//! `InitStrategy::Seeded(S)`, attaches a free-running clock on
//! `top.clock` (as `simtrace` does), runs N clock cycles, and prints
//! the phase table recorded by the `sim-profile` feature's counters
//! (`virdant::sim::profile`).  This is the measurement instrument for
//! simulator optimization work (see `PERF-IDEAS.md` item 1); kernel
//! profilers are blocked in the common sandbox.
//!
//! Build and run:
//!
//!     cargo build --release -p virdant --example simprof --features sim-profile
//!     target/release/examples/simprof --project DIR --cycles N --seed S
//!
//! The table is meaningful phase-versus-phase within one run: each
//! timed phase carries a fixed per-entry `Instant` overhead, so
//! absolute totals are inflated and only comparisons between phases
//! (and between configurations measured the same way) are
//! informative.  See `sim-bench/README.md` for the column meanings
//! and the estimator used for the sampled rows.

use std::cell::Cell;
use std::path::PathBuf;
use std::rc::Rc;

use virdant::sim::{Clock, InitStrategy, Sim};
use virdant::util::{check_db, db_from_dir_with_lib};

struct Args {
    project: PathBuf,
    cycles: u64,
    seed: u64,
}

const USAGE: &str = "\
usage: simprof --project DIR [--cycles N] [--seed S]
  --project DIR   project directory (containing src/ and virdant.toml)
  --cycles N      number of clock cycles to run (default 100000)
  --seed S        seed for InitStrategy::Seeded (default 0)";

fn parse_args() -> Args {
    let mut args = Args {
        project: PathBuf::from("."),
        cycles: 100_000,
        seed: 0,
    };
    let mut argv = std::env::args().skip(1);
    while let Some(arg) = argv.next() {
        let mut value_for = |flag: &str| {
            argv.next().unwrap_or_else(|| {
                eprintln!("simprof: {flag} expects a value\n{USAGE}");
                std::process::exit(2);
            })
        };
        match arg.as_str() {
            "--project" => args.project = PathBuf::from(value_for("--project")),
            "--cycles" => args.cycles = value_for("--cycles").parse().unwrap_or_else(|_| {
                eprintln!("simprof: --cycles expects a number\n{USAGE}");
                std::process::exit(2);
            }),
            "--seed" => args.seed = value_for("--seed").parse().unwrap_or_else(|_| {
                eprintln!("simprof: --seed expects a number\n{USAGE}");
                std::process::exit(2);
            }),
            _ => {
                eprintln!("simprof: unknown argument {arg:?}\n{USAGE}");
                std::process::exit(2);
            }
        }
    }
    args
}

#[cfg(feature = "sim-profile")]
mod table {
    use super::*;
    use virdant::sim::profile::Snapshot;

    /// Absolute nanoseconds, in the most readable unit for the
    /// magnitude.
    fn fmt_ns(ns: u64) -> String {
        if ns >= 10_000_000 {
            format!("{:.2} ms", ns as f64 / 1e6)
        } else if ns >= 10_000 {
            format!("{:.2} us", ns as f64 / 1e3)
        } else {
            format!("{ns} ns")
        }
    }

    /// Calibrate `eps`: the interval the two `Instant` reads of one
    /// timer entry embed past the measured body.  Measured as the
    /// mean interval of back-to-back read pairs, which contain no
    /// body at all.
    fn calibrate_read_ns() -> f64 {
        const N: usize = 2000;
        let mut total = 0u64;
        for _ in 0..N {
            let t0 = std::time::Instant::now();
            total += t0.elapsed().as_nanos() as u64;
        }
        total as f64 / N as f64
    }

    /// Estimate a sampled category's total time: exact call count x
    /// (mean sampled interval - eps), floored at zero.  See the
    /// estimator note in `virdant/src/sim/profile.rs`.
    fn est(call_count: u64, sample_ns: u64, samples: u64, eps: f64) -> u64 {
        if samples == 0 {
            return 0;
        }
        let mean = sample_ns as f64 / samples as f64;
        let per_call = (mean - eps).max(0.0);
        (call_count as f64 * per_call) as u64
    }

    /// The derived floor: per-cycle nanoseconds excluding the
    /// scheduler (pop + sampled push estimate), the watcher sweep,
    /// the whole measured flow settlement, and the measured commit
    /// work (the rising-edge transfer loops scale with the design,
    /// so they are not part of a fixed floor).  What remains is the
    /// flow-independent per-cycle overhead: event dispatch, callback
    /// bookkeeping, lock/transfer glue, and the run loop itself.  The
    /// residual also carries the instrumentation's own per-entry read
    /// cost (see the entries note printed with the table), so treat
    /// it as an upper bound on the true floor.
    fn floor_ns(snap: &Snapshot, push_est: u64, commit_est: u64) -> u64 {
        snap.run_ns
            .saturating_sub(snap.sched_ns)
            .saturating_sub(push_est)
            .saturating_sub(snap.dispatch_ns)
            .saturating_sub(snap.flow_ns)
            .saturating_sub(commit_est)
    }

    pub fn print(snap: &Snapshot, build: &Snapshot, cycles: u64, args: &Args) {
        let eps = calibrate_read_ns();
        let per_cycle =
            |ns: u64| if cycles > 0 { ns as f64 / cycles as f64 } else { 0.0 };
        let eval_est = est(snap.evals_total, snap.eval_sample_ns, snap.eval_samples, eps);
        let set_val_est =
            est(snap.set_val_calls, snap.commit_sample_ns, snap.commit_samples, eps);
        let xfer_est =
            est(snap.rising_edges, snap.xfer_sample_ns, snap.xfer_samples, eps);
        let commit_est = set_val_est.saturating_add(xfer_est);
        let push_est =
            est(snap.sched_push_calls, snap.push_sample_ns, snap.push_samples, eps);

        println!(
            "simprof: project={} seed={} cycles={} read_cal={eps:.1} ns",
            args.project.display(),
            args.seed,
            cycles,
        );
        println!();
        println!("  phase             total         per-cycle");
        let row = |name: &str, ns: u64| {
            println!("  {name:<17} {:>12} {:>9.1} ns", fmt_ns(ns), per_cycle(ns));
        };
        row("run total", snap.run_ns);
        row("scheduler", snap.sched_ns);
        row("sched push (est)", push_est);
        row("flow", snap.flow_ns);
        row("  eval (est)", eval_est);
        row("  commit (est)", commit_est);
        row("  cache lookup", snap.cache_lookup_ns);
        row("  cache clear", snap.cache_clear_ns);
        row("sweep", snap.dispatch_ns);
        row("floor", floor_ns(snap, push_est, commit_est));
        println!();
        println!(
            "  counts: events={} clock_edges={} pushes={} dirty_pops={} driver_evals={} (all vm, interp counter retired) const_skip={} epoch_skips={} sched_cache_hits={} sched_cache_misses={}",
            snap.events_popped,
            snap.clock_edges,
            snap.sched_push_calls,
            snap.dirty_pops,
            snap.evals_total,
            snap.const_skips,
            snap.epoch_skips,
            snap.schedule_cache_hits,
            snap.schedule_cache_misses,
        );
        println!(
            "          stage_c: replays={} aborts={} activations={} (cycle_replays/cycle_aborts/cycle_activations)",
            snap.cycle_replays,
            snap.cycle_aborts,
            snap.cycle_activations,
        );
        println!(
            "          jit_v2: compiled={} calls={} fallbacks={} (jit_phases_compiled/jit_phase_calls/jit_phase_fallbacks)",
            snap.jit_phases_compiled,
            snap.jit_phase_calls,
            snap.jit_phase_fallbacks,
        );
        println!(
            "          flow_calls={} (dirty_pops/flow_calls = {:.1} avg entry size)",
            snap.flow_calls,
            if snap.flow_calls > 0 { snap.dirty_pops as f64 / snap.flow_calls as f64 } else { 0.0 },
        );
        println!(
            "          set_val={}/{} transfers={}/{} rising_edges={} watcher_passes={} watcher_fires={}",
            snap.set_val_calls,
            snap.set_val_writes,
            snap.transfers,
            snap.transfer_writes,
            snap.rising_edges,
            snap.dispatch_passes,
            snap.watcher_fires,
        );
        println!(
            "          samples: eval={} set_val={} push={} xfer_loop={}",
            snap.eval_samples,
            snap.commit_samples,
            snap.push_samples,
            snap.xfer_samples,
        );
        let entries = snap.events_popped + snap.flow_calls + snap.dispatch_passes;
        println!(
            "  notes: eval is inside flow; commit covers set_val writes everywhere plus the\n           rising-edge transfer loops; floor carries the instrument's own cost\n           ({} timed entries, {:.1}/cycle, ~2 reads each), so treat it as an upper\n           bound and compare it phase-vs-phase across runs.",
            entries,
            if cycles > 0 { entries as f64 / cycles as f64 } else { 0.0 },
        );
        println!(
            "  build: flow={:.2} ms eval_est={:.2} ms topo_designs={} topo_fallbacks={}",
            build.flow_ns as f64 / 1e6,
            build.evals_total as f64 * build.eval_sample_ns as f64
                / build.eval_samples.max(1) as f64 / 1e6,
            build.topo_designs,
            build.topo_fallbacks,
        );
    }
}

#[cfg(not(feature = "sim-profile"))]
fn print_table(cycles: u64, args: &Args) {
    eprintln!(
        "simprof: compiled without --features sim-profile; no phase counters available"
    );
    eprintln!(
        "simprof: ran {} cycles over {} (seed {})",
        cycles,
        args.project.display(),
        args.seed,
    );
}

fn main() {
    let args = parse_args();

    let source_dir = args.project.join("src");
    let db = db_from_dir_with_lib(source_dir, "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        eprintln!("simprof: {} failed to build", args.project.display());
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let Some(top) = symboltable.resolve((&b"top::Top"[..]).into()) else {
        eprintln!("simprof: top symbol top::Top not found");
        std::process::exit(1);
    };

    let mut sim = Sim::new(&db, top.id(), InitStrategy::Seeded(args.seed));

    let Some(clock) = sim.try_resolve("top.clock") else {
        eprintln!("simprof: no `top.clock` signal to attach a clock to");
        std::process::exit(1);
    };
    sim.attach_clock(clock, Clock::with_period_ps(10_000));

    // Construction and clock attachment run their own settlements;
    // exclude them from the run-loop table.
    #[cfg(feature = "sim-profile")]
    let build = virdant::sim::profile::snapshot();
    #[cfg(feature = "sim-profile")]
    virdant::sim::profile::reset();

    let cycles_seen: Rc<Cell<u64>> = Rc::new(Cell::new(0));
    let cycles_cb = cycles_seen.clone();
    let target = args.cycles;
    sim.on_clock(clock, Box::new(move |sim| {
        let cycle = cycles_cb.get() + 1;
        cycles_cb.set(cycle);
        if cycle >= target {
            sim.finish();
        }
    }));

    sim.run().unwrap();

    #[cfg(feature = "sim-profile")]
    table::print(
        &virdant::sim::profile::snapshot(),
        &build,
        cycles_seen.get(),
        &args,
    );
    #[cfg(not(feature = "sim-profile"))]
    print_table(cycles_seen.get(), &args);
}
