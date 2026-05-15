//! simtest — throwaway integration sandbox for the virdant simulator.
//!
//! See SIM_DESIGN.md.  `main()` dispatches on argv[1] to one of several
//! testbench functions; each loads a real Virdant design, builds a
//! `Sim`, and exercises whatever simulator capability is being prototyped.

use std::sync::Arc;

use virdant::sim::{Event, Sim};
use virdant::util::{check_db, db_from_file_with_lib};

/// Iteration 1 testbench: schedule a small handful of events on top of
/// a real `Sim` built from `examples/lfsr.vir`, then drain the queue.
/// `Sim::run()` only prints each event as it fires; nothing simulates
/// LFSR state yet.  The point is to verify (a) `Sim::new` still works
/// end-to-end on a real design and (b) events fire in (time, FIFO) order.
fn lfsr_trace() {
    // Cargo runs examples with cwd = workspace root.
    let db = db_from_file_with_lib("examples/lfsr.vir", "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        panic!("lfsr.vir failed diagnostics");
    }

    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"lfsr::Lfsr".into()).unwrap();

    let mut sim = Sim::new(Arc::new(db), top.id());

    sim.schedule(Event::StartOfSimulation);

    sim.run().unwrap();
}

fn main() {
    let arg = std::env::args().nth(1).unwrap_or_else(|| "lfsr_trace".into());
    match arg.as_str() {
        "lfsr_trace" => lfsr_trace(),
        other => {
            eprintln!("unknown testbench: {other}");
            eprintln!("known testbenches:");
            eprintln!("  lfsr_trace");
            std::process::exit(2);
        }
    }
}
