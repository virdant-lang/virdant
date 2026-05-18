use std::sync::Arc;

use virdant::sim::{Sim, Value};
use virdant::util::{check_db, db_from_file_with_lib};

fn sim() -> Sim {
    let db = db_from_file_with_lib("examples/buffer.vir", "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"buffer::Top".into()).unwrap();
    Sim::new(Arc::new(db), top.id())
}

fn main() {
    let mut sim = sim();

    let clock = sim.signal("top.clock");
    let reset = sim.signal("top.reset");
    let out   = sim.signal("top.out");

    sim.add_clock(clock, 10_000);

    sim.at_start(Box::new(move |sim| {
        let mut lock = sim.lock();
        lock.set(reset, Value::Bit(true));
    }));
    sim.after(10_000, Box::new(move |sim| {
        let mut lock = sim.lock();
        lock.set(reset, Value::Bit(false));
    }));
    sim.after(1000_000, Box::new(|sim| sim.finish()));

    sim.on_clock(clock, Box::new(move |sim| {
        println!("[t={}ps] clock edge: out = {:?}", sim.now(), sim.get(out));
    }));

    sim.run().unwrap();
}
