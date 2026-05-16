//! Native VCD dumper for `Sim`.
//!
//! Public entry point: `Sim::attach_vcd`.  Header and an initial
//! `$dumpvars` snapshot are written synchronously before the call
//! returns; per-signal `on_change` watchers are registered to write
//! subsequent value-change lines.  A final `flush()` runs from
//! `Sim::run`'s end-of-simulation hook.
//!
//! Bit layout of aggregate values lives entirely in `encode()`.  Change
//! that one function to change how structs / unions / enums appear on
//! the wire.

use std::cell::{Cell, RefCell};
use std::io::Write;
use std::rc::Rc;

use bstr::{BString, ByteSlice};

use crate::analysis::elaboration::SignalId;
use crate::analysis::symbols::SymbolId;
use crate::common::{TypeScheme, Width};
use crate::db::Db;
use crate::sim::eval::Value;
use crate::sim::sim::Sim;
use crate::types::Type;

struct DumpSig {
    id: SignalId,
    code: String,
    width: Width,
}

pub(super) fn attach<W: Write + 'static>(sim: &mut Sim, writer: W) {
    let db = sim.db();
    let elab = sim.elaboration();

    let mut sigs: Vec<(DumpSig, BString)> = Vec::new();
    for comp in elab.components() {
        let width = type_width(&comp.typ(), &db);
        if width == 0 { continue; }
        let code = id_code(sigs.len());
        sigs.push((DumpSig { id: comp.id(), code, width }, comp.path().clone()));
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
            write_change(&mut *w, &s.code, s.width, &encode(&sim.get(s.id), &db));
        }
        writeln!(w, "$end").unwrap();
    }

    for (s, _) in sigs {
        let code = s.code;
        let width = s.width;
        let writer_cb = writer.clone();
        let last_time_cb = last_time.clone();
        let db_cb = db.clone();
        let sig_id = s.id;
        sim.on_change(sig_id, Box::new(move |s| {
            let t = s.now();
            let mut w = writer_cb.borrow_mut();
            if last_time_cb.get() != Some(t) {
                writeln!(w, "#{t}").unwrap();
                last_time_cb.set(Some(t));
            }
            let v = s.get(sig_id);
            write_change(&mut *w, &code, width, &encode(&v, &db_cb));
        }));
    }

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

// === Bit layout seam ===========================================================
// Everything below is the single place that knows how runtime values map to VCD
// bit strings.  Mirror of `verilog::conversion::type_width` and the matching
// union layout `{padding, arg0, ..., argN, tag}` from MSB to LSB.

fn type_width(typ: &Type, db: &Db) -> Width {
    match typ {
        Type::Bit | Type::Clock => 1,
        Type::Word(w) => *w,
        Type::Usual(typedef_sym) => {
            let typedef = db.get_typedef(*typedef_sym);
            let symboltable = db.get_symboltable();
            let slots = symboltable.slots(*typedef_sym);
            match typedef.kind {
                TypeScheme::UnionDef => {
                    let tag_width: Width = slots.len().try_into().unwrap();
                    let max_payload: Width = slots.iter().map(|slot| {
                        let sig = db.get_ctor_signature(slot.id());
                        sig.parameters.iter()
                            .map(|(_n, pt)| type_width(pt, db))
                            .sum::<Width>()
                    }).max().unwrap_or(0);
                    tag_width + max_payload
                }
                TypeScheme::StructDef => {
                    db.get_struct_fields(*typedef_sym).iter()
                        .map(|f| f.typ.as_ref().map(|t| type_width(t, db)).unwrap_or(0))
                        .sum()
                }
                _ => typedef.width.unwrap_or_else(|| slots.len().try_into().unwrap()),
            }
        }
    }
}

/// Encode a runtime `Value` as an MSB-first sequence of VCD bit characters.
/// Output length equals `type_width(value.typ(), db)`.
fn encode(value: &Value, db: &Db) -> String {
    let total = type_width(&value.typ(), db);
    match value {
        Value::X(_) => "x".repeat(total as usize),
        Value::Z(_) => "z".repeat(total as usize),
        Value::Bit(b) => if *b { "1".into() } else { "0".into() },
        Value::Word(w, v) => format_word(*v, *w),
        Value::Ctor(typ, ctor_sym, args) => encode_ctor(typ, *ctor_sym, args, db),
    }
}

fn format_word(v: u64, width: Width) -> String {
    let mut s = String::with_capacity(width as usize);
    for i in (0..width).rev() {
        s.push(if (v >> (i as u32)) & 1 != 0 { '1' } else { '0' });
    }
    s
}

fn encode_ctor(typ: &Type, ctor_sym: SymbolId, args: &[Value], db: &Db) -> String {
    let Type::Usual(typedef_sym) = typ else {
        return "x".repeat(type_width(typ, db) as usize);
    };
    let typedef = db.get_typedef(*typedef_sym);
    match typedef.kind {
        TypeScheme::UnionDef => {
            let symboltable = db.get_symboltable();
            let slots = symboltable.slots(*typedef_sym);
            let slot_index = slots.iter().position(|s| s.id() == ctor_sym).unwrap();
            let tag_width: Width = slots.len().try_into().unwrap();
            let max_payload: Width = slots.iter().map(|slot| {
                let sig = db.get_ctor_signature(slot.id());
                sig.parameters.iter().map(|(_n, pt)| type_width(pt, db)).sum::<Width>()
            }).max().unwrap_or(0);
            let mut payload = String::new();
            for arg in args {
                payload.push_str(&encode(arg, db));
            }
            let payload_width = payload.len() as Width;
            let padding = max_payload.saturating_sub(payload_width);
            let mut out = String::with_capacity((tag_width + max_payload) as usize);
            for _ in 0..padding { out.push('x'); }
            out.push_str(&payload);
            out.push_str(&format_word(1u64 << slot_index, tag_width));
            out
        }
        TypeScheme::EnumDef => {
            let width = typedef.width.unwrap_or(0);
            let val = typedef.enumerant_values.get(&ctor_sym).copied().unwrap_or(0);
            format_word(val, width)
        }
        _ => "x".repeat(type_width(typ, db) as usize),
    }
}
