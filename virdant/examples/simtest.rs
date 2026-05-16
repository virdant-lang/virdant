//! simtest — throwaway integration sandbox for the virdant simulator.
//!
//! `main()` dispatches on argv[1] to one of several testbench functions;
//! each loads a real Virdant design, builds a `Sim`, and exercises
//! whatever simulator capability is being prototyped.

use std::cell::RefCell;
use std::rc::Rc;
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

    let clock = sim.signal("top.clock");
    let reset = sim.signal("top.reset");
    let out   = sim.signal("top.out");

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

/// Same stimulus as `lfsr_run`, but record every `out` transition and
/// assert against the known Galois-LFSR sequence (polynomial 0x1D,
/// reset value 0xFF).  Rising edges fall on t = 5_000 + k * 10_000;
/// reset deasserts at t=10_000, so the t=5_000 edge latches the reset
/// value 255 and subsequent edges advance the LFSR.
fn lfsr_check() {
    let mut sim = lfsr_sim();

    let expected: Vec<(u64, Value)> = [
        (5_000,  255u64),
        (15_000, 227),
        (25_000, 219),
        (35_000, 171),
        (45_000,  75),
        (55_000, 150),
        (65_000,  49),
        (75_000,  98),
        (85_000, 196),
        (95_000, 149),
    ].into_iter().map(|(t, v)| (t, Value::Word(8, v))).collect();
    let expected_len = expected.len();

    let clock = sim.signal("top.clock");
    let reset = sim.signal("top.reset");
    let out   = sim.signal("top.out");

    sim.add_clock(clock, 10_000);

    let observed: Rc<RefCell<Vec<(u64, Value)>>> = Rc::new(RefCell::new(Vec::new()));
    let observed_cb = observed.clone();
    sim.on_change(out, Box::new(move |sim| {
        println!("[t={}ps] out = {:?}", sim.now(), sim.get(out));
        let mut observed_list = observed_cb.borrow_mut();
        observed_list.push((sim.now(), sim.get(out)));
        if observed_list.len() == expected_len {
            sim.finish();
        }
    }));

    sim.set(reset, Value::Bit(true));
    sim.after(10_000, Box::new(move |sim| {
        sim.set(reset, Value::Bit(false));
    }));
    sim.after(100_000, Box::new(|sim| sim.finish()));

    sim.run().unwrap();

    let actual = observed.borrow().clone();
    assert_eq!(actual, expected, "LFSR output sequence mismatch");
    println!("lfsr_check: {} transitions matched expected sequence", actual.len());
}

/// Test `on_clock` callback: fires on every rising edge of the clock.
fn lfsr_on_clock() {
    let mut sim = lfsr_sim();

    let clock = sim.signal("top.clock");
    let reset = sim.signal("top.reset");

    sim.add_clock(clock, 10_000);

    let clock_edge_count: Rc<RefCell<u64>> = Rc::new(RefCell::new(0));
    let count_cb = clock_edge_count.clone();
    sim.on_clock(clock, Box::new(move |sim| {
        let mut count = count_cb.borrow_mut();
        *count += 1;
        println!("[t={}ps] clock rising edge #{}", sim.now(), count);
    }));

    sim.set(reset, Value::Bit(true));
    sim.after(10_000, Box::new(move |sim| {
        sim.set(reset, Value::Bit(false));
    }));
    sim.after(100_000, Box::new(|sim| sim.finish()));

    sim.run().unwrap();

    let final_count = *clock_edge_count.borrow();
    println!("lfsr_on_clock: detected {} clock rising edges", final_count);
    assert!(final_count > 0, "on_clock should have fired at least once");
}

fn main() {
    let arg = std::env::args().nth(1).unwrap_or_else(|| "lfsr_check".into());
    match arg.as_str() {
        "lfsr_trace" => lfsr_trace(),
        "lfsr_run"   => lfsr_run(),
        "lfsr_check" => lfsr_check(),
        "lfsr_on_clock" => lfsr_on_clock(),
        other => {
            eprintln!("unknown testbench: {other}");
            eprintln!("known testbenches:");
            eprintln!("  lfsr_trace");
            eprintln!("  lfsr_run");
            eprintln!("  lfsr_check");
            eprintln!("  lfsr_on_clock");
            std::process::exit(2);
        }
    }
}
