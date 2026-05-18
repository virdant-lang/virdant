//! Native VCD dumper for `Sim`.
//!
//! Public entry point: `Sim::attach_vcd`.  Header and an initial
//! `$dumpvars` snapshot are written synchronously before the call
//! returns; per-signal `on_change` watchers are registered to write
//! subsequent value-change lines.  A final `flush()` runs from
//! `Sim::run`'s end-of-simulation hook.
//!
//! Bit layout of aggregate values lives entirely in `TypeEncoder::encode`.
//! Change that one method to change how structs / unions / enums appear on
//! the wire.
//!
//! `TypeEncoder` is built once from `&Db` inside `Sim::new` and stored on
//! `Sim`.  The `on_change` closures clone it (cheap: the inner map is behind
//! an `Arc`) instead of retaining `Arc<Db>`, so the database may be dropped
//! as soon as `Sim` is constructed.

use std::cell::{Cell, RefCell};
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;

use bstr::{BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::analysis::symbols::SymbolId;
use crate::common::{TypeScheme, Width, WordValue};
use crate::db::Db;
use crate::sim::eval::Value;
use crate::sim::sim::Sim;
use crate::types::Type;

// TypeLayoutIndex / TypeLayout

/// Index of precomputed bit-layout data for every `Type::Usual` reachable
/// from the elaboration.  Built once from `&Db` inside `Circuit::new`;
/// cloned cheaply (inner map is `Arc`) into every `on_change` closure
/// registered by `attach_vcd`.
#[derive(Clone)]
pub(super) struct TypeLayoutIndex {
    layouts: Arc<IndexMap<SymbolId, TypeLayout>>,
}

/// Bit-layout description for a single `Type::Usual` typedef.
#[derive(Clone)]
pub(super) enum TypeLayout {
    Union {
        /// `(ctor_symbol_id, param_types)` - one entry per slot, in
        /// symbol-table order so `slot_index` in `encode_ctor` is stable.
        slots: Vec<(SymbolId, Vec<Type>)>,
    },
    Enum {
        width: Width,
        enumerant_values: IndexMap<SymbolId, WordValue>,
    },
    Struct {
        field_types: Vec<Type>,
    },
    Other(Width),
}

impl TypeLayoutIndex {
    /// Build a `TypeLayoutIndex` covering every `Type::Usual` reachable from
    /// the elaborated component types.
    pub(super) fn build(db: &Db, elab: &Elaboration) -> Self {
        let mut layouts: IndexMap<SymbolId, TypeLayout> = IndexMap::new();
        let mut queue: Vec<SymbolId> = Vec::new();

        for comp in elab.components() {
            seed_type(&comp.typ(), &mut queue);
        }

        let symboltable = db.get_symboltable();

        while let Some(sym) = queue.pop() {
            if layouts.contains_key(&sym) {
                continue;
            }
            let layout = build_type_layout(db, &symboltable, sym, &mut queue);
            layouts.insert(sym, layout);
        }

        TypeLayoutIndex { layouts: Arc::new(layouts) }
    }

    pub(super) fn type_width(&self, typ: &Type) -> Width {
        match typ {
            Type::Bit | Type::Clock | Type::Reset => 1,
            Type::Word(w) => *w,
            Type::Usual(sym) => self.usual_width(*sym),
        }
    }

    fn usual_width(&self, sym: SymbolId) -> Width {
        match &self.layouts[&sym] {
            TypeLayout::Union { slots } => {
                let tag_width: Width = slots.len().try_into().unwrap();
                let max_payload: Width = slots
                    .iter()
                    .map(|(_, types)| types.iter().map(|t| self.type_width(t)).sum::<Width>())
                    .max()
                    .unwrap_or(0);
                tag_width + max_payload
            }
            TypeLayout::Struct { field_types } => {
                field_types.iter().map(|t| self.type_width(t)).sum()
            }
            TypeLayout::Enum { width, .. } | TypeLayout::Other(width) => *width,
        }
    }

    pub(super) fn encode(&self, value: &Value) -> String {
        let total = self.type_width(&value.typ());
        match value {
            Value::X(_) => "x".repeat(total as usize),
            Value::Z(_) => "z".repeat(total as usize),
            Value::Bit(b) => if *b { "1".into() } else { "0".into() },
            Value::Word(w, v) => format_word(*v, *w),
            Value::Ctor(typ, ctor_sym, args) => self.encode_ctor(typ, *ctor_sym, args),
        }
    }

    fn encode_ctor(&self, typ: &Type, ctor_sym: SymbolId, args: &[Value]) -> String {
        let Type::Usual(typedef_sym) = typ else {
            return "x".repeat(self.type_width(typ) as usize);
        };
        match &self.layouts[typedef_sym] {
            TypeLayout::Union { slots } => {
                let tag_width: Width = slots.len().try_into().unwrap();
                let max_payload: Width = slots
                    .iter()
                    .map(|(_, types)| types.iter().map(|t| self.type_width(t)).sum::<Width>())
                    .max()
                    .unwrap_or(0);
                let slot_index = slots.iter().position(|(s, _)| *s == ctor_sym).unwrap();

                let mut payload = String::new();
                for arg in args {
                    payload.push_str(&self.encode(arg));
                }
                let payload_width = payload.len() as Width;
                let padding = max_payload.saturating_sub(payload_width);
                let mut out = String::with_capacity((tag_width + max_payload) as usize);
                for _ in 0..padding { out.push('0'); }
                out.push_str(&payload);
                out.push_str(&format_word(1u64 << slot_index, tag_width));
                out
            }
            TypeLayout::Enum { width, enumerant_values } => {
                let val = enumerant_values.get(&ctor_sym).copied().unwrap_or(0);
                format_word(val, *width)
            }
            _ => "x".repeat(self.type_width(typ) as usize),
        }
    }
}

/// Build the `TypeLayout` for a single typedef `sym`, seeding `queue` with
/// any `Type::Usual` symbols found in its definition.
fn build_type_layout(
    db: &Db,
    symboltable: &crate::analysis::symbols::SymbolTable,
    sym: SymbolId,
    queue: &mut Vec<SymbolId>,
) -> TypeLayout {
    let typedef = db.get_typedef(sym);
    let slots = symboltable.slots(sym);
    match typedef.kind {
        TypeScheme::UnionDef => {
            let slot_infos = slots
                .iter()
                .map(|slot| {
                    let sig = db.get_ctor_signature(slot.id());
                    let param_types: Vec<Type> = sig
                        .parameters
                        .iter()
                        .map(|(_name, t)| { seed_type(t, queue); t.clone() })
                        .collect();
                    (slot.id(), param_types)
                })
                .collect();
            TypeLayout::Union { slots: slot_infos }
        }
        TypeScheme::StructDef => {
            let field_types: Vec<Type> = db
                .get_struct_fields(sym)
                .into_iter()
                .filter_map(|f| f.typ)
                .map(|t| { seed_type(&t, queue); t })
                .collect();
            TypeLayout::Struct { field_types }
        }
        TypeScheme::EnumDef => {
            let width = typedef.width.unwrap_or_else(|| slots.len().try_into().unwrap());
            TypeLayout::Enum { width, enumerant_values: typedef.enumerant_values.clone() }
        }
        _ => {
            let width = typedef.width.unwrap_or_else(|| slots.len().try_into().unwrap());
            TypeLayout::Other(width)
        }
    }
}

/// Push any `SymbolId` embedded in `typ` onto `queue` for processing.
fn seed_type(typ: &Type, queue: &mut Vec<SymbolId>) {
    if let Type::Usual(sym) = typ {
        queue.push(*sym);
    }
}

// attach

struct DumpSig {
    id: SignalId,
    code: String,
    width: Width,
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
            write_change(&mut *w, &s.code, s.width, &layout_index.encode(&sim.get(s.id)));
        }
        writeln!(w, "$end").unwrap();
    }

    for (s, _) in sigs {
        let code = s.code;
        let width = s.width;
        let writer_cb = writer.clone();
        let last_time_cb = last_time.clone();
        let layout_index_cb = layout_index.clone();
        let sig_id = s.id;
        sim.on_change(sig_id, Box::new(move |s| {
            let t = s.now();
            let mut w = writer_cb.borrow_mut();
            if last_time_cb.get() != Some(t) {
                writeln!(w, "#{t}").unwrap();
                last_time_cb.set(Some(t));
            }
            let v = s.get(sig_id);
            write_change(&mut *w, &code, width, &layout_index_cb.encode(&v));
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

fn format_word(v: u64, width: Width) -> String {
    let mut s = String::with_capacity(width as usize);
    for i in (0..width).rev() {
        s.push(if (v >> (i as u32)) & 1 != 0 { '1' } else { '0' });
    }
    s
}
