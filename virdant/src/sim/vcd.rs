//! Native VCD dumper for `Sim`.
//!
//! Public entry point: `Sim::attach_vcd`.  Header and an initial
//! `$dumpvars` snapshot are written synchronously before the call
//! returns; per-signal `on_change` watchers are registered to write
//! subsequent value-change lines.  A final `flush()` runs from
//! `Sim::run`'s end-of-simulation hook.
//!
//! Values are bit patterns, so the wire dump is a straight render of
//! each signal's bits.  `TypeLayoutIndex` supplies the per-signal type
//! widths (including user-defined aggregate types).

use std::cell::{Cell, RefCell};
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
    /// Timestamp offset (ps) for this signal's changes: the sim's
    /// `vcd_offset_ps` for every signal except `Clock`-typed ones
    /// (which stamp at their true time — the clock is the reference
    /// the offset is measured from).  Lets a driver show tick-caused
    /// changes a visible sliver after the edge, without the engine
    /// modelling delay at all.
    offset: u64,
}

pub(super) fn attach<W: Write + 'static>(sim: &mut Sim, writer: W) {
    let layout_index = sim.type_layout_index().clone();
    let elab = sim.elaboration();

    let offset = sim.vcd_offset_ps();
    let mut sigs: Vec<(DumpSig, BString)> = Vec::new();
    for comp in elab.components() {
        // A memory's scalar value is a change-detection version
        // counter, not signal data; its words are not VCD-traced.
        if comp.component_kind() == crate::common::ComponentKind::Memory { continue; }
        let width = layout_index.type_width(&comp.typ());
        if width == 0 { continue; }
        let code = id_code(sigs.len());
        let sig_offset = if matches!(comp.typ(), crate::types::Type::Clock) { 0 } else { offset };
        sigs.push((DumpSig { id: comp.id(), code, width, offset: sig_offset }, comp.path().clone()));
    }
    sigs.sort_by(|a, b| a.1.cmp(&b.1));

    let writer = Rc::new(RefCell::new(writer));
    {
        let mut w = writer.borrow_mut();
        writeln!(w, "$timescale 1ps $end").unwrap();
        write_scope_tree(&mut *w, &sigs);
        writeln!(w, "$enddefinitions $end").unwrap();
    }

    let last_time: Rc<Cell<Option<u64>>> = Rc::new(Cell::new(None));
    {
        let mut w = writer.borrow_mut();
        let t = sim.now();
        writeln!(w, "#{t}").unwrap();
        last_time.set(Some(t));
        writeln!(w, "$dumpvars").unwrap();
        for (s, _) in &sigs {
            write_change(&mut *w, &s.code, s.width, &sim.get(s.id).to_bit_string());
        }
        writeln!(w, "$end").unwrap();
    }

    // Register watchers clocks-first: within one settlement, a clock
    // stamps at its true time while offset signals stamp `offset` later,
    // so the clock must emit first or the trace's timestamps would go
    // backwards.  (The driver must also keep clock events at least
    // `offset` apart across settlements — true for poke's ticks.)
    let mut ordered: Vec<&(DumpSig, BString)> = sigs.iter().collect();
    ordered.sort_by_key(|(s, _)| s.offset != 0);
    for (s, _) in ordered {
        let code = s.code.clone();
        let width = s.width;
        let offset = s.offset;
        let writer_cb = writer.clone();
        let last_time_cb = last_time.clone();
        let sig_id = s.id;
        sim.on_change(sig_id, Box::new(move |s| {
            let t = s.now() + offset;
            let mut w = writer_cb.borrow_mut();
            if last_time_cb.get() != Some(t) {
                writeln!(w, "#{t}").unwrap();
                last_time_cb.set(Some(t));
            }
            let v = s.get(sig_id);
            write_change(&mut *w, &code, width, &v.to_bit_string());
        }));
    }


    // Time advancement with no value changes (`advance_time`: poke's
    // `advance`, and the half-period steps inside `tick`) would
    // otherwise leave no trace: value-change watchers only fire on
    // changes, so the file would end at the last change and renderers
    // would truncate the final segment.  A marker callback stamps the
    // moved-to time into the trace, deduplicated against the same
    // `last_time` the change callbacks use.
    let writer_marker = writer.clone();
    let last_time_marker = last_time.clone();
    sim.set_time_marker(Box::new(move |t| {
        // A marker stamps pure time advancement; never write it before
        // an already-emitted offset stamp (a poke's +offset change can
        // outlive a same-time marker, e.g. `advance 0`).
        let behind = last_time_marker.get().is_some_and(|last| t < last);
        if !behind && last_time_marker.get() != Some(t) {
            let mut w = writer_marker.borrow_mut();
            writeln!(w, "#{t}").unwrap();
            last_time_marker.set(Some(t));
        }
    }));

    let writer_end = writer.clone();
    sim.at_end(Box::new(move |_| {
        let _ = writer_end.borrow_mut().flush();
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
    if width == 1 {
        writeln!(w, "{bits}{code}").unwrap();
    } else {
        writeln!(w, "b{bits} {code}").unwrap();
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
