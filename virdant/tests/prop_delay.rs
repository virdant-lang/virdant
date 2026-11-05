//! Integration tests for the propagation-delay model
//! (`plans/gates_and_prop`): VCD stamping, buffer reordering, and the
//! eager panic checks.
//!
//! Each test builds a small design from inline Virdant source (written
//! to a temp directory), attaches a VCD dumper over an in-memory
//! buffer, and asserts on the emitted `#` timestamps and change lines.
//! Delays are asserted in the stamped absolute times (`settlement time
//! + pole × unit_ps`).

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use virdant::sim::{BitPat, Clock, InitStrategy, Sim, Value};
use virdant::util::{check_db, db_from_files_with_lib};

/// An in-memory VCD sink: the dumper writes through `Rc<RefCell<_>>`,
/// so the test reads the same buffer afterwards.
#[derive(Clone, Default)]
struct Capture(Rc<RefCell<Vec<u8>>>);

impl std::io::Write for Capture {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl Capture {
    fn text(&self) -> String {
        String::from_utf8(self.0.borrow().clone()).unwrap()
    }
}

/// The bundled lib dir (`lib/` next to the `virdant` crate) — resolved
/// explicitly because the `Sim` runtime's exe-relative `LIB_DIR` only
/// works for installed binaries.
fn lib_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().join("lib")
}

/// Write the test design to a temp directory and build a checked `Db`.
/// Package names come from file stems (e.g. `top.vir` → package `top`).
fn db_for(tag: &str, files: &[(&str, &str)]) -> virdant::db::Db {
    let dir = std::env::temp_dir().join(format!("virdant-prop-test-{tag}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let paths: Vec<PathBuf> = files
        .iter()
        .map(|(name, text)| {
            let path = dir.join(name);
            std::fs::write(&path, text).unwrap();
            path
        })
        .collect();
    let db = db_from_files_with_lib(paths, lib_dir());
    if let Err(diagnostics) = check_db(&db) {
        for diagnostic in diagnostics.iter() {
            eprintln!("{diagnostic:?}");
        }
        panic!("test design failed diagnostics");
    }
    db
}

/// A `Sim` over `top::Top` in the given design.
fn sim_for(tag: &str, files: &[(&str, &str)]) -> Sim {
    let db = db_for(tag, files);
    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"top::Top".into()).unwrap();
    Sim::new(&db, top.id(), InitStrategy::Random)
}

/// The text following the `#stamp` header line, up to the next `#`
/// header (or the end) — the change lines that landed on that stamp.
fn stamp_section<'a>(out: &'a str, stamp: u64) -> &'a str {
    let marker = format!("#{stamp}\n");
    let start = out.find(&marker).unwrap_or_else(|| panic!("missing #{stamp} stamp:\n{out}"));
    let rest = &out[start + marker.len()..];
    match rest.find("\n#") {
        Some(end) => &rest[..end],
        None => rest,
    }
}

/// Drive an input port or register to `value`.
fn poke(sim: &mut Sim, path: &str, value: u64) {
    let id = sim.signal(path);
    let typ = sim.component_type(id);
    let typ_id = sim.component_type_id(id);
    let width = sim.layouts().type_width(&typ);
    let mut lock = sim.lock();
    lock.set(id, Value::new(typ_id, BitPat::from_u64(width, value)));
}

/// A word adder: pole of `s` is 20 × 4 = 80 units.
const ADD4: &str = r#"
pub mod Top {
    incoming a : Word[4]
    incoming b : Word[4]
    outgoing s : Word[4] dependson a, b

    s := a + b
}
"#;

/// Same, with a clock port for unit-scaled tests.
const ADD4_CLOCKED: &str = r#"
pub mod Top {
    incoming clock : Clock
    incoming a : Word[4]
    incoming b : Word[4]
    outgoing s : Word[4] dependson a, b

    s := a + b
}
"#;

/// A deep chain (three ripple adds, pole 480) beside a shallow XOR
/// (pole 10) — the out-of-order stamp hazard.
const DEEP_SHALLOW: &str = r#"
pub mod Top {
    incoming x : Word[8]
    incoming y : Word[8]
    outgoing deep : Word[8] dependson x, y
    outgoing shallow : Word[8] dependson x, y

    deep := ((x + y) + x) + y
    shallow := x ^ y
}
"#;

/// A reduction: pole of `a` is 10 · ⌈log₂ 8⌉ = 30 units.
const ANY8: &str = r#"
pub mod Top {
    incoming x : Word[8]
    outgoing a : Bit dependson x

    a := any(x)
}
"#;

/// A match whose arm body stacks operator delays on the mux tree.
const MATCH_ADD: &str = r#"
pub mod Top {
    incoming x : Word[4]
    outgoing y : Word[4] dependson x

    y := match x {
        with 0 => 3
        else   => x + 1
    }
}
"#;

/// A registered output: `out` follows `buffer` (a reg, pole 0), so
/// both change exactly at the clock edge.
const BUFFER8: &str = r#"
pub mod Top {
    incoming clock : Clock
    incoming inp : Word[8]
    outgoing out : Word[8]

    reg buffer : Word[8] {
        it <= inp
    }
    out := buffer
}
"#;

/// A 64-bit adder under a 1 ns clock: pole 128 units × 10 ps = 1280 ps
/// overflows the 1000 ps period.
const ADD64_SLOW_CLOCK: &str = r#"
pub mod Top {
    incoming clock : Clock
    incoming a : Word[64]
    incoming b : Word[64]
    outgoing s : Word[64] dependson a, b

    s := a + b
}
"#;

/// Two clocks, for the second-clock panic test.
const TWO_CLOCKS: &str = r#"
pub mod Top {
    incoming clock : Clock
    incoming clock2 : Clock
    incoming a : Word[4]
    outgoing s : Word[4] dependson a

    s := a + a
}
"#;

const RF_EXT: &str = r#"
pub ext mod Rf {
    incoming clock : Clock
    incoming addr : Word[4]
    outgoing data : Word[8]
}
"#;

const RF_TOP: &str = r#"
pub mod Top {
    incoming clock : Clock
    incoming addr : Word[4]
    outgoing data : Word[8]

    mod rf0 of rf::Rf {
        it.clock := clock
        it.addr := addr
        data := it.data
    }
}
"#;

#[test]
fn vcd_stamps_combinational_pole_fallback_unit() {
    // No clock attached: the fallback unit is 1 ps, so the adder's
    // pole of 80 units stamps the sum change at t+80.
    let mut sim = sim_for("comb", &[("top.vir", ADD4)]);
    sim.with_propagation(true);
    let cap = Capture::default();
    sim.attach_vcd(cap.clone());

    // Zero the inputs first (register/sum init values are random): the
    // sum's zeroing lands at its pole stamp #8.
    poke(&mut sim, "top.a", 0);
    poke(&mut sim, "top.b", 0);
    sim.advance_time(100); // the zeroed sum lands at #8 (not asserted: the
                           // random init may already be 0)

    // Then a=1, b=2 at t=100: the sum lands at 100 + 81 = #181 (the
    // user-written ports cost 1 unit each, the adder 80).
    poke(&mut sim, "top.a", 1);
    poke(&mut sim, "top.b", 2);
    sim.advance_time(200);

    let out = cap.text();
    // Inputs are sources (stamped at their settlement time); the sum
    // carries the cone's pole: `a=1, b=2 → s=3` at #181.
    assert_eq!(stamp_section(&out, 181).matches("b0011 #").count(), 1);
}

#[test]
fn vcd_stamps_with_clock_unit() {
    // Clock period 10_000 ps → unit = 100 ps → stamp = 80 × 100 = 8000.
    let mut sim = sim_for("clocked", &[("top.vir", ADD4_CLOCKED)]);
    sim.with_propagation(true);
    let cap = Capture::default();
    sim.attach_vcd(cap.clone());
    let clock = sim.signal("top.clock");
    sim.attach_clock(clock, Clock::with_period_ps(10_000));

    sim.at_start(Box::new(|sim| {
        // Zero first (sum init is random), then the real stimulus.
        poke(sim, "top.a", 0);
        poke(sim, "top.b", 0);
    }));
    sim.at(15_000, Box::new(|sim| {
        poke(sim, "top.a", 1);
        poke(sim, "top.b", 2);
    }));
    sim.after(60_000, Box::new(|sim| sim.finish()));
    sim.run().unwrap();

    let out = cap.text();
    // Unit = period/400 = 25 ps (the model's universal ÷4): the
    // a=1, b=2 sum (written at t=15_000) lands at 15_000 + 81 × 25
    // = #17025.
    assert_eq!(stamp_section(&out, 17_025).matches("b0011 $").count(), 1);
    // The first clock edge stamps at the edge time itself (pole 0).
    assert!(stamp_section(&out, 5_000).contains("1!"), "missing clock edge at #5000:\n{out}");
}

#[test]
fn vcd_reorders_out_of_order_stamps() {
    // deep (pole 48) and shallow (pole 1) change together at t=100;
    // deep's stamp (148) is buffered past the next flush, then a
    // shallow change at t=110 (stamp 111) arrives *later* but stamps
    // *earlier*.  The emitted # sequence must stay monotonic.
    let mut sim = sim_for("reorder", &[("top.vir", DEEP_SHALLOW)]);
    sim.with_propagation(true);
    let cap = Capture::default();
    sim.attach_vcd(cap.clone());

    // Deterministic baseline: settle everything to 0 (the random-init
    // values differ per run, so their change events are not asserted).
    poke(&mut sim, "top.x", 0);
    poke(&mut sim, "top.y", 0);
    sim.advance_time(500); // flush ≤ 500: deep's #480 and shallow's #10 land

    // Phase 1 at t=500: shallow → 3 at #511; deep → 6 at #981.
    poke(&mut sim, "top.x", 1);
    poke(&mut sim, "top.y", 2);
    sim.advance_time(300); // t=800: flush ≤ 800 — #511 lands; deep stays buffered
    assert_eq!(stamp_section(&cap.text(), 511).matches("b00000011 $").count(), 1);

    // Phase 2 at t=800: shallow → 2 at #811 (arrives after deep's
    // #981 event but stamps earlier); deep → 8 at #1281.
    poke(&mut sim, "top.y", 3);
    sim.advance_time(600); // t=1400: flush ≤ 1400 — #811, #981, #1281 in order

    let out = cap.text();
    // Ids follow elaboration order: deep=#, shallow=$, x=!, y=".
    assert_eq!(stamp_section(&out, 811).matches("b00000010 $").count(), 1);
    // Two deep settlements at t=500 (x first, against y=0: deep=2;
    // then y=2: deep=6) both land under #981, in settlement order.
    assert_eq!(stamp_section(&out, 981), "b00000010 #\nb00000110 #");
    assert_eq!(stamp_section(&out, 1281), "b00001000 #");
    // Monotonic, correctly ordered: the t=500-buffered deep event
    // (#981) emitted only after the later-arriving shallow change
    // (#811) — the reorder the buffer exists for.
    let at_511 = out.find("#511\n").unwrap();
    let at_811 = out.find("#811\n").unwrap();
    let at_981 = out.find("#981\n").unwrap();
    let at_1281 = out.find("#1281\n").unwrap();
    assert!(at_511 < at_811 && at_811 < at_981 && at_981 < at_1281, "stamps out of order:\n{out}");
}

#[test]
fn propagation_off_keeps_legacy_stamps() {
    // Without the flag, stamps stay at settlement times: the deep
    // chain's #48 (and the #1/#11 stamps) never appear.
    let mut sim = sim_for("off", &[("top.vir", DEEP_SHALLOW)]);
    let cap = Capture::default();
    sim.attach_vcd(cap.clone());

    poke(&mut sim, "top.x", 1);
    poke(&mut sim, "top.y", 2);
    sim.advance_time(10);
    poke(&mut sim, "top.y", 3);
    sim.advance_time(100);

    let out = cap.text();
    // Legacy stamps: only settlement times (#0, #10, #110) appear —
    // never a pole offset.
    assert!(!out.contains("#510"), "unexpected #510 stamp:\n{out}");
    assert!(!out.contains("#480"), "unexpected #480 stamp:\n{out}");
    assert!(out.contains("#10\n"), "expected the #10 settlement stamp:\n{out}");
}

#[test]
fn vcd_stamps_reduction_pole() {
    // any(Word[8]): ⌈log₂ 8⌉ = 3 units → #3 with the 1 ps fallback.
    let mut sim = sim_for("any", &[("top.vir", ANY8)]);
    sim.with_propagation(true);
    let cap = Capture::default();
    sim.attach_vcd(cap.clone());

    // Zero first: `a` settles to false deterministically (its change
    // event may not fire if the random init was already false).
    poke(&mut sim, "top.x", 0);
    sim.advance_time(100);

    poke(&mut sim, "top.x", 5);
    sim.advance_time(200);

    let out = cap.text();
    assert!(out.contains("#131\n1\""), "missing #131 any() stamp:\n{out}");
}

#[test]
fn vcd_stamps_match_arm_stack() {
    // The arm body's adder (20 × 4 = 80 units) dominates the mux tree
    // (2 arms → 20 units): pole = 80 with the fallback unit.  (The
    // arm references `x` directly, a source, so no stacking on the
    // mux; a bound-variable body would inherit subject + mux delay.)
    let mut sim = sim_for("match", &[("top.vir", MATCH_ADD)]);
    sim.with_propagation(true);
    let cap = Capture::default();
    sim.attach_vcd(cap.clone());

    poke(&mut sim, "top.x", 0);
    sim.advance_time(100); // y settles to 3 (at #8; not asserted: the
                           // random init may already be 3)

    poke(&mut sim, "top.x", 1);
    sim.advance_time(200); // y := 2 at 100 + 81 = #181

    let out = cap.text();
    assert_eq!(stamp_section(&out, 181).matches("b0010 \"").count(), 1);
}

#[test]
fn vcd_stamps_registered_output_at_edge() {
    // Registered output: buffer and out both change exactly at the
    // clock edge (pole 0 through the register source).
    let mut sim = sim_for("buffer", &[("top.vir", BUFFER8)]);
    sim.with_propagation(true);
    let cap = Capture::default();
    sim.attach_vcd(cap.clone());
    let clock = sim.signal("top.clock");
    sim.attach_clock(clock, Clock::with_period_ps(10_000));

    sim.at_start(Box::new(|sim| poke(sim, "top.inp", 0)));
    sim.at(15_000, Box::new(|sim| poke(sim, "top.inp", 7)));
    sim.after(45_000, Box::new(|sim| sim.finish()));
    sim.run().unwrap();

    let out = cap.text();
    // (Ids follow elaboration order: buffer=$, clock=!, inp=", out=#.)
    // At #5000 the clock rises (a Clock-typed source stamps at its
    // true time — it is the reference its own edges are measured from);
    // buffer latches 0 at clock-to-Q +1 — the buffer event only fires
    // if the random init was non-zero, so assert just the clock.
    assert!(stamp_section(&out, 5_000).contains("1!"), "missing clock rise at #5000:\n{out}");
    // At #15_000 the clock rises again (edges at 5k/15k/25k…, stamped
    // at their true times).  At #15_025 the inp=7 poke (fired just
    // before the toggle at the same timestamp, +1 user write = 25 ps)
    // and the buffer latch (clock-to-Q +1) share the stamp; out (a
    // direct connection off a register: pole 2 units = 50 ps) lands
    // after.
    assert!(stamp_section(&out, 15_000).contains("1!"), "missing clock rise at #15000:\n{out}");
    assert_eq!(stamp_section(&out, 15_025).matches("b00000111").count(), 2, "missing inp/buffer at #15025:\n{out}");
    assert_eq!(stamp_section(&out, 15_050).matches("b00000111").count(), 1, "missing out latch at #15050:\n{out}");
}

#[test]
#[should_panic(expected = "different period")]
fn panics_on_second_clock_with_different_period() {
    let mut sim = sim_for("twoclocks", &[("top.vir", TWO_CLOCKS)]);
    let clock = sim.signal("top.clock");
    let clock2 = sim.signal("top.clock2");
    sim.attach_clock(clock, Clock::with_period_ps(10_000));
    sim.with_propagation(true);
    sim.attach_clock(clock2, Clock::with_period_ps(20_000));
}

#[test]
#[should_panic(expected = "propagation delay overflow")]
fn panics_on_pole_overflow() {
    let mut sim = sim_for("overflow", &[("top.vir", ADD64_SLOW_CLOCK)]);
    sim.with_propagation(true);
    // Pole 128 units × (1000/100) ps = 1280 ps ≥ 1000 ps period.
    let clock = sim.signal("top.clock");
    sim.attach_clock(clock, Clock::with_period_ps(1_000));
}

#[test]
#[should_panic(expected = "no delay model for builtin ext modules")]
fn panics_on_builtin_ext_modules() {
    let mut sim = sim_for("builtins", &[("rf.vir", RF_EXT), ("top.vir", RF_TOP)]);
    sim.with_propagation(true);
}
