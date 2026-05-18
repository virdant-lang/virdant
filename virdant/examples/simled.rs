use virdant::sim::{Clock, Sim, Value};
use virdant::util::{check_db, db_from_file_with_lib};

fn sim() -> Sim {
    let db = db_from_file_with_lib("examples/extmods.vir", "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"extmods::Top".into()).unwrap();
    Sim::new(&db, top.id())
}

fn main() {
    let mut sim = sim();

    let clock = sim.signal("top.clock");
    let reset = sim.signal("top.reset");
    let led_clock = sim.signal("top.led.clock");
    let led_inp = sim.signal("top.led.inp");
    let out = sim.signal("top.out");

    sim.attach_clock(clock, Clock::with_period_ps(10_000));

    sim.after(100_000, Box::new(|sim| sim.finish()));

    sim.at_start(Box::new(move |sim| {
        let mut lock = sim.lock();
        lock.set(reset, Value::Bit(true));
        drop(lock);
        sim.after(10_000, Box::new(move |sim| {
            let mut lock = sim.lock();
            lock.set(reset, Value::Bit(false));
        }));
    }));

    sim.on_change(out, Box::new(move |sim| {
        println!("out changed to {:?}", sim.get(out));
    }));

    sim.on_clock(led_clock, Box::new(move |sim| {
        println!("TICK top.led.inp = {:?}", sim.get(led_inp));
        let led_out = sim.signal("top.led.out");
        let inp_val = sim.get(led_inp);
        let mut lock = sim.lock();
        lock.set(led_out, inp_val);
    }));

    sim.run().unwrap();
}
