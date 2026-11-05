//! Native VCD dumper for `Sim`.
//!
//! Public entry point: `Sim::attach_vcd`.  Header and an initial
//! `$dumpvars` snapshot are written synchronously before the call
//! returns; per-signal `on_change` watchers are registered to write
//! subsequent value changes.  A final flush runs from `Sim::run`'s
//! end-of-simulation hook, and a `Drop` tail flush covers sessions
//! that never `run()` (the poke tester).
//!
//! Values are bit patterns, so the wire dump is a straight render of
//! each signal's bits.  `TypeLayoutIndex` supplies the per-signal type
//! widths (including user-defined aggregate types).
//!
//! ## Propagation-delay stamping
//!
//! With `Sim::with_propagation(true)`, each signal's value changes are
//! stamped at `settlement time + pole delay` (see `sim/delay.rs`)
//! instead of the settlement time.  Because different signals carry
//! different delays, change events can become ready out of order, and
//! VCD requires monotonically non-decreasing `#` timestamps — so
//! events are buffered in a min-heap and emitted in stamp order.
//!
//! Flush rule: all future value changes have a *base* time ≥ the next
//! time the simulation clock moves, so an event stamped at or before
//! the current time (checked on every time advance, via the time
//! marker) can never be preceded by a later-arriving earlier stamp and
//! is safe to emit.  The end-of-simulation hook and `Drop` drain the
//! remainder.  With stamping off, the dumper writes exactly as before
//! this feature existed (no buffering).

use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::io::Write;
use std::rc::Rc;

use bstr::{BString, ByteSlice};

use crate::analysis::elaboration::SignalId;
use crate::common::Width;
use crate::sim::sim::Sim;

// attach

struct DumpSig {
    id: SignalId,
    code: String,
    width: Width,
}

/// One buffered value change: the absolute time it becomes visible
/// (`settlement time + pole delay`), a global sequence number for
/// same-stamp ordering (per-signal order is push order), and the
/// pre-rendered VCD change line.
struct Pending {
    stamp: u64,
    seq: u64,
    line: String,
}

impl PartialEq for Pending {
    fn eq(&self, other: &Self) -> bool {
        self.stamp == other.stamp && self.seq == other.seq
    }
}
impl Eq for Pending {}
impl PartialOrd for Pending {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Pending {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.stamp.cmp(&other.stamp).then(self.seq.cmp(&other.seq))
    }
}

/// Emission state shared by the change watchers, the time marker, the
/// end-of-simulation hook, and the `Drop` tail flush.
struct Emitter<W: Write> {
    writer: Rc<RefCell<W>>,
    state: RefCell<EmitterState>,
}

struct EmitterState {
    /// Buffered change events, min-ordered by `(stamp, seq)`.
    pending: BinaryHeap<Reverse<Pending>>,
    seq: u64,
    /// The last `#` timestamp written; every writer path dedupes
    /// against it.
    last_time: Option<u64>,
}

impl<W: Write> Emitter<W> {
    /// Emit every buffered event stamped at or before `threshold`, in
    /// `(stamp, seq)` order.  Events stamped later stay buffered: a
    /// future settlement may still produce an earlier stamp.
    fn flush_up_to(&self, threshold: u64) {
        let mut state = self.state.borrow_mut();
        let mut w = self.writer.borrow_mut();
        while let Some(Reverse(p)) = state.pending.peek() {
            if p.stamp > threshold {
                break;
            }
            let Reverse(p) = state.pending.pop().unwrap();
            if state.last_time != Some(p.stamp) {
                writeln!(w, "#{}", p.stamp).unwrap();
                state.last_time = Some(p.stamp);
            }
            write!(w, "{}", p.line).unwrap();
        }
    }

    /// Drain everything buffered.
    fn flush_all(&self) {
        self.flush_up_to(u64::MAX);
    }
}

impl<W: Write> Drop for Emitter<W> {
    /// Tail flush for sessions that never run `Sim::run` (the poke
    /// tester, which exits without an end-of-simulation hook): the
    /// buffered remainder is still written, in stamp order.
    fn drop(&mut self) {
        self.flush_all();
    }
}

pub(super) fn attach<W: Write + 'static>(sim: &mut Sim, writer: W) {
    let layout_index = sim.type_layout_index().clone();
    let elab = sim.elaboration();

    let mut sigs: Vec<(DumpSig, BString)> = Vec::new();
    for comp in elab.components() {
        let width = layout_index.type_width(&comp.typ());
        if width == 0 { continue; }
        let code = id_code(sigs.len());
        sigs.push((DumpSig { id: comp.id(), code, width }, comp.path().clone()));
    }
    sigs.sort_by(|a, b| a.1.cmp(&b.1));

    let emitter = Rc::new(Emitter {
        writer: Rc::new(RefCell::new(writer)),
        state: RefCell::new(EmitterState {
            pending: BinaryHeap::new(),
            seq: 0,
            last_time: None,
        }),
    });

    {
        let mut w = emitter.writer.borrow_mut();
        writeln!(w, "$timescale 1ps $end").unwrap();
        write_scope_tree(&mut *w, &sigs);
        writeln!(w, "$enddefinitions $end").unwrap();
    }

    {
        let mut w = emitter.writer.borrow_mut();
        let t = sim.now();
        writeln!(w, "#{t}").unwrap();
        emitter.state.borrow_mut().last_time = Some(t);
        writeln!(w, "$dumpvars").unwrap();
        for (s, _) in &sigs {
            write_change(&mut *w, &s.code, s.width, &sim.get(s.id).to_bit_string());
        }
        writeln!(w, "$end").unwrap();
    }

    for (s, _) in sigs {
        let code = s.code;
        let width = s.width;
        let emitter_cb = emitter.clone();
        let sig_id = s.id;
        sim.on_change(sig_id, Box::new(move |s| {
            let bits = s.get(sig_id).to_bit_string();
            if s.propagation() {
                // Buffered: the change becomes visible at the stamp,
                // not at the settlement time.
                let stamp = s.now() + s.prop_offset_ps(sig_id);
                let mut state = emitter_cb.state.borrow_mut();
                let seq = state.seq;
                state.seq += 1;
                state.pending.push(Reverse(Pending {
                    stamp,
                    seq,
                    line: render_change(&code, width, &bits),
                }));
            } else {
                // Legacy immediate path, byte-identical to the
                // pre-propagation dumper.
                let t = s.now();
                let mut w = emitter_cb.writer.borrow_mut();
                let mut state = emitter_cb.state.borrow_mut();
                if state.last_time != Some(t) {
                    writeln!(w, "#{t}").unwrap();
                    state.last_time = Some(t);
                }
                write_change(&mut *w, &code, width, &bits);
            }
        }));
    }

    // Time advancement with no value changes (`advance_time`: poke's
    // `advance`, and the half-period steps inside `tick`; with
    // propagation on, also every scheduler pop inside `run`) would
    // otherwise leave no trace: value-change watchers only fire on
    // changes, so the file would end at the last change and renderers
    // would truncate the final segment.  The marker also acts as the
    // buffering flush trigger: all future changes have a base time at
    // or after the newly-moved-to time, so every buffered event
    // stamped at or before it is final and can be emitted now.
    let emitter_marker = emitter.clone();
    sim.set_time_marker(Box::new(move |t| {
        emitter_marker.flush_up_to(t);
        let mut w = emitter_marker.writer.borrow_mut();
        let mut state = emitter_marker.state.borrow_mut();
        if state.last_time != Some(t) {
            writeln!(w, "#{t}").unwrap();
            state.last_time = Some(t);
        }
    }));

    let emitter_end = emitter.clone();
    sim.at_end(Box::new(move |_| {
        emitter_end.flush_all();
        let _ = emitter_end.writer.borrow_mut().flush();
    }));
}

fn write_scope_tree<W: Write>(w: &mut W, sigs: &[(DumpSig, BString)]) {
    let mut current: Vec<BString> = Vec::new();
    for (s, path) in sigs {
        let segs: Vec<&[u8]> = path.split(|b| *b == b'.').collect();
        let (scope, leaf) = segs.split_at(segs.len() - 1);
        let mut prefix = 0;
        while prefix < current.len()
            && prefix < scope.len()
            && current[prefix].as_slice() == scope[prefix]
        {
            prefix += 1;
        }
        for _ in prefix..current.len() {
            writeln!(w, "$upscope $end").unwrap();
        }
        current.truncate(prefix);
        for seg in &scope[prefix..] {
            writeln!(w, "$scope module {} $end", seg.as_bstr()).unwrap();
            current.push(BString::from(seg.to_vec()));
        }
        writeln!(
            w,
            "$var wire {} {} {} $end",
            s.width,
            s.code,
            leaf[0].as_bstr()
        ).unwrap();
    }
    for _ in 0..current.len() {
        writeln!(w, "$upscope $end").unwrap();
    }
}

fn write_change<W: Write>(w: &mut W, code: &str, width: Width, bits: &str) {
    write!(w, "{}", render_change(code, width, bits)).unwrap();
}

/// One rendered change line (with its trailing newline).
fn render_change(code: &str, width: Width, bits: &str) -> String {
    if width == 1 {
        format!("{bits}{code}\n")
    } else {
        format!("b{bits} {code}\n")
    }
}

fn id_code(mut n: usize) -> String {
    let mut s = Vec::new();
    loop {
        s.push((n % 94) as u8 + b'!');
        n /= 94;
        if n == 0 { break; }
    }
    s.reverse();
    String::from_utf8(s).unwrap()
}
