//! simtest — throwaway integration sandbox for the virdant simulator.
//!
//! `main()` dispatches on argv[1] to one of several testbench functions;
//! each loads a real Virdant design, builds a `Sim`, and exercises
//! whatever simulator capability is being prototyped.

use std::sync::Arc;

use virdant::sim::{Sim, Value};
use virdant::util::{check_db, db_from_file_with_lib};

/// Build a `Sim` for `examples/lfsr.vir`.
fn lfsr_sim() -> Sim {
    let db = db_from_file_with_lib("examples/lfsr.vir", "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        panic!("lfsr.vir failed diagnostics");
    }

    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"lfsr::Lfsr".into()).unwrap();
    Sim::new(Arc::new(db), top.id())
}

/// Minimal lifecycle test: register at_start / at_end callbacks and run
/// with an empty time queue.  The simulator should fire start, see no
/// timed callbacks, then fire end.
fn lfsr_trace() {
    let mut sim = lfsr_sim();
    sim.at_start(Box::new(|sim| println!("[t={}ps] start", sim.now())));
    sim.at_end(Box::new(|sim| println!("[t={}ps] end", sim.now())));
    sim.run().unwrap();
}

/// Drive the LFSR with a real clock, pulse reset for one cycle, watch
/// `out` for value changes, and finish at t=100_000.
///
/// Timing (clock period = 10 ns = 10_000 ps, first rising edge at t=5000):
///   t=0      reset := 1, clock := 0  (synchronous setup)
///   t=5000   rising edge  -> r latches reset value (255), out := 255
///   t=10000  reset := 0   (and clock falls, no transfer)
///   t=15000  rising edge  -> r advances from 255 to next LFSR state
///   ...
///   t=100000 finish
fn lfsr_run() {
    let mut sim = lfsr_sim();

    let clock = sim.resolve("top.clock");
    let reset = sim.resolve("top.reset");
    let out   = sim.resolve("top.out");

    sim.add_clock(clock, 10_000);

    sim.on_change(out, Box::new(move |sim| {
        println!("[t={}ps] out = {:?}", sim.now(), sim.get(out));
    }));

    sim.set(reset, Value::Bit(true));
    sim.after(10_000, Box::new(move |sim| {
        sim.set(reset, Value::Bit(false));
    }));
    sim.after(100_000, Box::new(|sim| sim.finish()));

    sim.run().unwrap();
}

fn main() {
    let arg = std::env::args().nth(1).unwrap_or_else(|| "lfsr_run".into());
    match arg.as_str() {
        "lfsr_trace" => lfsr_trace(),
        "lfsr_run"   => lfsr_run(),
        other => {
            eprintln!("unknown testbench: {other}");
            eprintln!("known testbenches:");
            eprintln!("  lfsr_trace");
            eprintln!("  lfsr_run");
            std::process::exit(2);
        }
    }
}
