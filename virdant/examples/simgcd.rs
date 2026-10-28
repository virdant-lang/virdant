use std::cell::RefCell;
use std::fs::File;
use std::io::BufWriter;
use std::rc::Rc;

use virdant::sim::{BitPat, Clock, InitStrategy, Sim, Value};
use virdant::types::Type;
use virdant::util::{check_db, db_from_dir_with_lib};

/// (a, b, expected_gcd)
fn word8(v: u64) -> Value {
    Value::new(Type::Word(8), BitPat::from_u64(8, v))
}

const CASES: &[(u64, u64, u64)] = &[
    // Usual cases
    (75,  145,   5),
    (12,    8,   4),
    (100,  75,  25),
    (48,   18,   6),
    // Corner cases
    (1,    1,    1),   // equal, minimum
    (255,  255, 255),  // equal, maximum
    (5,    0,    5),   // y = 0: Done after one Running cycle
    (0,    7,    7),   // x = 0
    (7,   11,    1),   // coprime primes
];

fn sim() -> Sim {
    let db = db_from_dir_with_lib("tests/gcd/src", "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"top::Top".into()).unwrap();
    Sim::new(&db, top.id(), InitStrategy::Random)
}

fn main() {
    let mut sim = sim();

    let clock  = sim.signal("top.clock");
    let reset  = sim.signal("top.reset");
    let x      = sim.signal("top.x");
    let y      = sim.signal("top.y");
    let fire   = sim.signal("top.fire");
    let result = sim.signal("top.result");
    let valid  = sim.signal("top.valid");

    sim.attach_clock(clock, Clock::with_period_ps(10_000));

    let vcd_path = "build/gcd.vcd";
    std::fs::create_dir_all("build").unwrap();
    sim.attach_vcd(BufWriter::new(File::create(vcd_path).unwrap()));
    println!("Dumping VCD to {vcd_path}");

    // started becomes true once reset is released so the on_clock
    // callback can ignore the first cycle where state is still unknown.
    let started: Rc<RefCell<bool>> = Rc::new(RefCell::new(false));
    let started_init = started.clone();
    let started_cb   = started.clone();

    // (test_idx, fired)
    //   fired=false  next on_clock should assert x/y/fire and move to fired=true
    //   fired=true   waiting for valid to go high
    let state: Rc<RefCell<(usize, bool)>> = Rc::new(RefCell::new((0, false)));
    let state_cb = state.clone();

    sim.at_start(Box::new(move |sim| {
        let mut lock = sim.lock();
        lock.set(reset, Value::from_bool(Type::Reset, true));
        lock.set(fire,  Value::from_bool(Type::Bit, false));
    }));

    sim.after(15_000, Box::new(move |sim| {
        let mut lock = sim.lock();
        lock.set(reset, Value::from_bool(Type::Reset, false));
        drop(lock);
        *started_init.borrow_mut() = true;
    }));

    sim.on_clock(clock, Box::new(move |sim| {
        if !*started_cb.borrow() {
            return;
        }

        let (idx, fired) = *state_cb.borrow();

        if !fired {
            let (a, b, _) = CASES[idx];
            {
                let mut lock = sim.lock();
                lock.set(x,    word8(a));
                lock.set(y,    word8(b));
                lock.set(fire, Value::from_bool(Type::Bit, true));
            }
            *state_cb.borrow_mut() = (idx, true);
        } else {
            // Deassert fire after the one cycle it was high.
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(Type::Bit, false));
            }

            if sim.get(valid).get_bit(0) {
                let (a, b, expected) = CASES[idx];
                let actual = sim.get(result).to_u64();
                assert_eq!(actual, expected,
                    "gcd({a}, {b}): expected {expected}, got {actual}");
                println!("gcd({a:3}, {b:3}) = {actual:3}  ✓");

                let next = idx + 1;
                if next >= CASES.len() {
                    sim.finish();
                } else {
                    *state_cb.borrow_mut() = (next, false);
                }
            }
        }
    }));

    sim.run().unwrap();
    println!("All {} GCD assertions passed.", CASES.len());
}
