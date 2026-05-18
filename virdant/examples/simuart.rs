use std::cell::RefCell;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

use virdant::sim::{Sim, Value};
use virdant::util::{check_db, db_from_dir_with_lib};

const CLOCK_HZ:  u64 = 12_000_000;
const BAUD_RATE: u64 = 9_600;

fn sim() -> Sim {
    let db = db_from_dir_with_lib("tests/uart/src", "lib");
    if let Err(diagnostics) = check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"uart::Top".into()).unwrap();
    Sim::new(Arc::new(db), top.id())
}

struct Decoder {
    decoding: bool,
}

fn main() {
    let mut sim = sim();

    let clock   = sim.signal("top.clock");
    let reset   = sim.signal("top.reset");
    let uart_tx = sim.signal("top.sender.uart_tx");
    let fin     = sim.signal("top.fin");

    sim.add_clock_hz(clock, CLOCK_HZ);

    let vcd_path = "build/uart.vcd";
    std::fs::create_dir_all("build").unwrap();
    sim.attach_vcd(BufWriter::new(File::create(vcd_path).unwrap()));
    println!("Dumping VCD to {vcd_path}");

    let clock_period_ps: u64 = 1_000_000_000_000 / CLOCK_HZ;
    let bit_period_ps:   u64 = 1_000_000_000_000 / BAUD_RATE;
    let half_bit_ps:     u64 = bit_period_ps / 2;

    sim.at_start(Box::new(move |sim| {
        let mut lock = sim.lock();
        lock.set(reset, Value::Bit(true));
    }));
    sim.at_end(Box::new(move |_sim| {
        println!();
    }));
    // Hold reset across the first clock posedge (which arrives at
    // half_period after t=0) so the registers actually load their
    // reset values.
    sim.after(2 * clock_period_ps, Box::new(move |sim| {
        let mut lock = sim.lock();
        lock.set(reset, Value::Bit(false));
    }));

    sim.on_change(fin, Box::new(move |sim| {
        if sim.get(fin) == Value::Bit(true) {
            sim.finish();
        }
    }));

    let decoder: Rc<RefCell<Decoder>> = Rc::new(RefCell::new(Decoder { decoding: false }));
    let decoder_cb = decoder.clone();
    sim.on_change(uart_tx, Box::new(move |sim| {
        if decoder_cb.borrow().decoding {
            return;
        }
        if sim.get(uart_tx) != Value::Bit(false) {
            return;
        }
        decoder_cb.borrow_mut().decoding = true;

        let acc: Rc<RefCell<u8>> = Rc::new(RefCell::new(0));
        for i in 0..8u8 {
            let acc_i = acc.clone();
            let delay = half_bit_ps + (i as u64 + 1) * bit_period_ps;
            if i < 7 {
                sim.after(delay, Box::new(move |sim| {
                    let bit = matches!(sim.get(uart_tx), Value::Bit(true));
                    *acc_i.borrow_mut() |= (bit as u8) << i;
                }));
            } else {
                let decoder_done = decoder_cb.clone();
                sim.after(delay, Box::new(move |sim| {
                    let bit = matches!(sim.get(uart_tx), Value::Bit(true));
                    *acc_i.borrow_mut() |= (bit as u8) << i;
                    let b = *acc_i.borrow();
                    let mut out = std::io::stdout();
                    out.write_all(&[b]).unwrap();
                    out.flush().unwrap();
                    decoder_done.borrow_mut().decoding = false;
                }));
            }
        }
    }));

    let wall_start = Instant::now();
    sim.run().unwrap();
    let wall_elapsed = wall_start.elapsed();

    let sim_elapsed_ps = sim.now();
    let sim_elapsed_s = sim_elapsed_ps as f64 / 1e12;
    let wall_elapsed_s = wall_elapsed.as_secs_f64();
    let ratio = sim_elapsed_s / wall_elapsed_s;
    eprintln!(
        "Simulated {sim_elapsed_s:.6}s in {wall_elapsed_s:.6}s wallclock (ratio {ratio:.5})",
    );
}
