//! Bit-layout for Virdant's type system — the one shared implementation.
//!
//! Every value is a bit pattern (see `sim::bits`); this module defines
//! how each type's inhabitants map onto bits:
//!
//! - `Bit` / `Clock` / `Reset` are 1 bit.
//! - `Word[w]` is `w` bits.
//! - A struct is the concatenation of its fields' bits, **first field
//!   in the lowest bits** (matching the Verilog backend's `ExprField`
//!   bit-offset arithmetic), filling the width exactly.
//! - A union is a one-hot tag (bit `i` set selects slot `i`) in the low
//!   `tag_width = slots.len()` bits, with the selected slot's payload
//!   bits above (first parameter at the MSB end of the payload area,
//!   zero padding up to the widest slot).
//! - An enum is the enumerant's value in the low `tag_width` bits,
//!   payload bits above (same payload layout as unions).
//!
//! A bit pattern is a *valid* inhabitant of its type when it decodes
//! cleanly: the tag must be a known one-hot/enumerant value and (for
//! unions/enums) the padding above the payload must be zero.
//!
//! `TypeLayoutIndex` is keyed by the full instantiated type
//! (`Type::Usual` with its type arguments), so a generic type
//! instantiated at two different argument lists gets two layouts.
//! Layouts store member types *substituted* with those arguments, so
//! widths are mono.
//!
//! Two entry points share the same construction core:
//!
//! - [`TypeLayoutIndex::build_layouts`] (implemented in `sim::layout`)
//!   seeds from an [`Elaboration`]'s component types, exactly as the
//!   simulator always has;
//! - [`TypeLayoutIndex::for_types`] seeds from an explicit list of
//!   types, so tools which work on types alone — like the `vir value`
//!   encode/decode CLI — need no elaboration at all.

use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::symbols::SymbolId;
use crate::common::Width;
use crate::db::Db;
use crate::sim::BitPat;
use crate::common::TypeScheme;
use crate::types::substitute::substitute;
use crate::types::typ::Type;
use crate::types::typedef::TypeDef;

/// One slot of a union/enum, or one field of a struct.  For struct
/// fields `param_types` has exactly one entry (the field type).
#[derive(Clone, Debug)]
pub struct Slot {
    pub symbol_id: SymbolId,
    pub name: BString,
    pub param_types: Vec<Type>,
}

/// A dense handle for an interned [`Type`].
///
/// Every `Type` reachable from the seeded types (layout keys, member
/// types, and every sim `Expr` node's type) is interned once while the
/// immutable structures are built; after that a `TypeId` is a plain
/// `u32`, so `Value`s can carry their type tag as four bytes instead
/// of a heap-allocated `Type` tree.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

/// The `Type` -> `TypeId` intern table.  Built once during index
/// construction (seeded from the elaboration or an explicit type list),
/// then frozen behind an `Arc` inside [`TypeLayoutIndex`]; runtime
/// access is read-only and allocation-free.
#[derive(Debug, Default)]
pub struct TypeInterner {
    ids: IndexMap<Type, TypeId>,
    types: Vec<Type>,
}

impl TypeInterner {
    pub fn new() -> TypeInterner {
        TypeInterner { ids: IndexMap::new(), types: Vec::new() }
    }

    /// The id for `typ`, interning it if this is the first sighting.
    /// (Construction-time only: the table is frozen once the index is
    /// finished.)
    pub fn intern(&mut self, typ: &Type) -> TypeId {
        if let Some(&id) = self.ids.get(typ) {
            return id;
        }
        let id = TypeId(self.types.len() as u32);
        self.types.push(typ.clone());
        self.ids.insert(typ.clone(), id);
        id
    }

    /// The id for a previously-interned `typ`.  Distinct types get
    /// distinct ids, so id equality is type equality.
    pub fn type_id(&self, typ: &Type) -> TypeId {
        *self.ids.get(typ).unwrap_or_else(|| {
            panic!("TypeInterner: type not interned: {typ:?}")
        })
    }

    /// The interned `Type` for `id`.
    pub fn type_of(&self, id: TypeId) -> &Type {
        &self.types[id.0 as usize]
    }
}

/// Intern `typ` and, recursively, every type embedded in its `Usual`
/// argument list (including primitive argument types, which `seed_type`
/// alone does not reach).
pub(crate) fn intern_all(typ: &Type, types: &mut TypeInterner) {
    types.intern(typ);
    if let Type::Usual(_, args) = typ {
        for arg in args {
            intern_all(arg, types);
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeLayoutKind {
    Union { slots: Vec<Slot> },
    Enum {
        tag_width: Width,
        enumerant_values: IndexMap<SymbolId, u64>,
        slots: Vec<Slot>,
    },
    Struct { fields: Vec<Slot> },
    Other(Width),
}

#[derive(Clone, Debug)]
pub(crate) struct TypeLayout {
    /// The typedef's (simple) name, for display.
    #[allow(dead_code)]
    name: BString,
    kind: TypeLayoutKind,
}

/// Index of precomputed bit-layout data for every user-defined type
/// instantiation reachable from the seed types, plus the `Type` intern
/// table (see [`TypeId`]).  Built once from `&Db`; cloned cheaply (both
/// inner structures behind an `Arc`).
#[derive(Clone, Debug)]
pub struct TypeLayoutIndex {
    layouts: Arc<IndexMap<Type, TypeLayout>>,
    types: Arc<TypeInterner>,
}

/// The bit width of `typ`, given the layout map.  `Type::Param` is
/// generic and never appears in mono artifacts; it yields 0.
pub(crate) fn layout_width(layouts: &IndexMap<Type, TypeLayout>, typ: &Type) -> Width {
    match typ {
        Type::Bit | Type::Clock | Type::Reset => 1,
        Type::Word(w) => *w,
        Type::Param(_) => 0,
        Type::Usual(..) => match layouts.get(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Union { slots }) => {
                let tag_width: Width = slots.len().try_into().unwrap();
                tag_width + max_payload_width(layouts, slots)
            }
            Some(TypeLayoutKind::Enum { tag_width, slots, .. }) => {
                *tag_width + max_payload_width(layouts, slots)
            }
            Some(TypeLayoutKind::Struct { fields }) => fields
                .iter()
                .map(|f| layout_width(layouts, &f.param_types[0]))
                .sum(),
            Some(TypeLayoutKind::Other(width)) => *width,
            None => 0,
        },
    }
}

fn max_payload_width(layouts: &IndexMap<Type, TypeLayout>, slots: &[Slot]) -> Width {
    slots
        .iter()
        .map(|s| s.param_types.iter().map(|t| layout_width(layouts, t)).sum::<Width>())
        .max()
        .unwrap_or(0)
}

impl TypeLayoutIndex {
    /// The bit width of a `Type` without an index (for primitive types;
    /// user types need an index — those never come through this path).
    pub fn type_width_of(typ: &Type) -> Width {
        match typ {
            Type::Bit | Type::Clock | Type::Reset => 1,
            Type::Word(w) => *w,
            _ => 0,
        }
    }

    /// Build an index covering every type instantiation reachable from
    /// `seeds` — the standalone entry point for tools which work from
    /// types alone (no elaboration): `vir value` encode/decode.
    ///
    /// Pre-interns `Word[w]` for every width up to 128 (plus the width
    /// of any wider seed), so literal handling always finds a `TypeId`.
    pub fn for_types(db: &Db, seeds: impl IntoIterator<Item = Type>) -> TypeLayoutIndex {
        let seeds: Vec<Type> = seeds.into_iter().collect();
        let mut types = TypeInterner::new();
        let layouts = build_layouts(db, &seeds, &mut types);
        TypeLayoutIndex::from_parts(layouts, types)
    }

    /// Freeze the built layout map and intern table into the shared,
    /// read-only index.
    pub(crate) fn from_parts(
        layouts: IndexMap<Type, TypeLayout>,
        types: TypeInterner,
    ) -> Self {
        TypeLayoutIndex { layouts: Arc::new(layouts), types: Arc::new(types) }
    }

    fn layout(&self, typ: &Type) -> Option<&TypeLayout> {
        self.layouts.get(typ)
    }

    /// The interned id for `typ`.  Panics if `typ` was never interned
    /// during index construction — every runtime value's type is seeded
    /// there, so this indicates a construction-order bug.
    pub fn type_id(&self, typ: &Type) -> TypeId {
        self.types.type_id(typ)
    }

    /// The interned `Type` for `id`.
    pub fn type_of(&self, id: TypeId) -> &Type {
        self.types.type_of(id)
    }

    fn layout_of(&self, typ: TypeId) -> Option<&TypeLayout> {
        self.layouts.get(self.types.type_of(typ))
    }

    /// The bit width of a value of `typ`.  `Type::Param` is generic and
    /// never appears in mono artifacts; it yields 0.
    pub fn type_width(&self, typ: &Type) -> Width {
        layout_width(&self.layouts, typ)
    }

    /// The fields of a struct type, in declaration order.
    pub fn fields(&self, typ: &Type) -> Option<&[Slot]> {
        match self.layout(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Struct { fields }) => Some(fields),
            _ => None,
        }
    }

    /// The kind of the layout of `typ`, for structural decisions
    /// (union vs enum vs struct vs primitive-ish `Other`).
    pub fn kind_of(&self, typ: &Type) -> Option<&TypeLayoutKind> {
        self.layout(typ).map(|l| &l.kind)
    }

    /// The declared (simple) name of `typ`'s typedef, for display.
    pub fn type_name(&self, typ: &Type) -> Option<&BStr> {
        self.layout(typ).map(|l| l.name.as_bstr())
    }

    /// Look up a union/enum constructor of the interned type `typ` by
    /// name: its symbol id and the types of its parameters, in
    /// declaration order.  `None` when `typ` is not a union/enum or has
    /// no constructor with that name.  Used by [`Sim::encode_ctor_by_name`].
    pub fn ctor_slot_by_name(&self, typ: TypeId, name: &str) -> Option<(SymbolId, Vec<Type>)> {
        let layout = self.layout_of(typ)?;
        let slots = match &layout.kind {
            TypeLayoutKind::Union { slots } => slots,
            TypeLayoutKind::Enum { slots, .. } => slots,
            _ => return None,
        };
        let slot = slots
            .iter()
            .find(|slot| slot.name.as_bstr() == name.as_bytes())?;
        Some((slot.symbol_id, slot.param_types.clone()))
    }

    /// The (substituted) slot list of a union/enum type.
    pub fn slots_of(&self, typ: &Type) -> Option<&[Slot]> {
        match self.layout(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Union { slots }) => Some(slots),
            Some(TypeLayoutKind::Enum { slots, .. }) => Some(slots),
            _ => None,
        }
    }

    /// The enumerant values of an enum type (`symbol id` -> value).
    pub fn enumerant_values_of(&self, typ: &Type) -> Option<&IndexMap<SymbolId, u64>> {
        match self.layout(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Enum { enumerant_values, .. }) => Some(enumerant_values),
            _ => None,
        }
    }

    /// Encode a constructor application as a bit pattern of `typ`:
    /// the tag in the low bits, the encoded parameters above (first
    /// parameter at the MSB end), zero padding up to the type width.
    pub fn encode_ctor(
        &self,
        typ: TypeId,
        ctor_sym: SymbolId,
        args: &[BitPat],
    ) -> Option<BitPat> {
        let layout = self.layout_of(typ)?;
        let (tag_width, tag) = match &layout.kind {
            TypeLayoutKind::Union { slots } => {
                let index = slots.iter().position(|s| s.symbol_id == ctor_sym)?;
                (slots.len().try_into().unwrap(), 1u64 << index)
            }
            TypeLayoutKind::Enum { tag_width, enumerant_values, .. } => {
                (*tag_width, *enumerant_values.get(&ctor_sym)?)
            }
            _ => return None,
        };
        let total = self.type_width(self.type_of(typ));

        let mut payload = BitPat::zeros(0);
        for arg in args {
            payload = payload.concat_msb_first(arg);
        }
        let payload_width = payload.width();

        let mut out = BitPat::zeros(total);
        for i in 0..tag_width as usize {
            out.set_bit(i, (tag >> i) & 1 != 0);
        }
        for i in 0..payload_width as usize {
            out.set_bit(tag_width as usize + i, payload.get_bit(i));
        }
        Some(out)
    }

    /// Decode a bit pattern of `typ` as a union/enum inhabitant.
    /// Returns the selected slot and the payload bit patterns for its
    /// parameters (substituted types), or `None` when the pattern is
    /// not a valid inhabitant (unknown tag, non-zero padding).
    pub fn decode_ctor(&self, typ: TypeId, bits: &BitPat) -> Option<(Slot, Vec<BitPat>)> {
        let layout = self.layout_of(typ)?;
        let (slots, tag_width, enumerant_values) = match &layout.kind {
            TypeLayoutKind::Union { slots } => {
                (slots, slots.len().try_into().unwrap(), None)
            }
            TypeLayoutKind::Enum { tag_width, enumerant_values, slots } => {
                (slots, *tag_width, Some(enumerant_values))
            }
            _ => return None,
        };
        let tag = bits.slice(0, tag_width).to_u64();

        let slot_index = match enumerant_values {
            Some(values) => slots
                .iter()
                .position(|s| values.get(&s.symbol_id).copied() == Some(tag))?,
            None => slots.iter().enumerate().find(|(i, _)| 1u64 << i == tag)?.0,
        };
        let slot = &slots[slot_index];

        // Payload area: bits [tag_width, tag_width + payload_width), zero
        // padding above.
        let payload_width: Width = slot.param_types.iter().map(|t| self.type_width(t)).sum();
        let total = self.type_width(self.type_of(typ));
        if total > tag_width + payload_width {
            let padding = bits.slice(tag_width + payload_width, total);
            if !padding.is_zero() {
                return None;
            }
        }

        // First parameter at the MSB end of the payload area.
        let mut payloads = Vec::with_capacity(slot.param_types.len());
        let mut hi = tag_width + payload_width;
        for param_type in &slot.param_types {
            let w = self.type_width(param_type);
            payloads.push(bits.slice(hi - w, hi));
            hi -= w;
        }
        Some((slot.clone(), payloads))
    }

    /// Decode a bit pattern of a struct type into its fields' bit
    /// patterns, in declaration order (first field in the lowest
    /// bits).  Returns `None` when `typ` is not a struct (or the
    /// pattern is the wrong width).
    pub fn decode_struct(&self, typ: &Type, bits: &BitPat) -> Option<Vec<BitPat>> {
        let fields: Vec<Slot> = self.fields(typ)?.to_vec();
        if bits.width() != self.type_width(typ) {
            return None;
        }
        let mut hi = bits.width();
        let mut out = Vec::with_capacity(fields.len());
        for field in fields.iter().rev() {
            let w = self.type_width(&field.param_types[0]);
            out.push(bits.slice(hi - w, hi));
            hi -= w;
        }
        out.reverse();
        Some(out)
    }

    /// Encode a struct literal as a bit pattern of `typ`: the fields
    /// given in declaration order (first field at the MSB end), zero
    /// padding below the lowest field if the declared field widths do
    /// not fill the type width.  `field_bits` must be in declaration
    /// order with each pattern matching its field's width.
    pub fn encode_struct(&self, typ: &Type, field_bits: &[BitPat]) -> Option<BitPat> {
        let fields = self.fields(typ)?;
        if fields.len() != field_bits.len() {
            return None;
        }
        let total = self.type_width(typ);
        let mut out = BitPat::zeros(total);
        let mut hi = total;
        for (field, bits) in fields.iter().zip(field_bits.iter()) {
            let w = self.type_width(&field.param_types[0]);
            if bits.width() != w {
                return None;
            }
            let lo = hi - w;
            for i in 0..w as usize {
                out.set_bit(lo as usize + i, bits.get_bit(i));
            }
            hi = lo;
        }
        Some(out)
    }

    /// True when `bits` is a valid inhabitant of `typ`.
    pub fn valid(&self, typ: TypeId, bits: &BitPat) -> bool {
        match self.type_of(typ) {
            Type::Bit | Type::Clock | Type::Reset | Type::Word(_) => true,
            Type::Param(_) => false,
            Type::Usual(..) => match self.layout_of(typ).map(|l| &l.kind) {
                Some(TypeLayoutKind::Struct { .. }) => {
                    match self.decode_struct(self.type_of(typ), bits) {
                        Some(field_bits) => {
                            let fields = self.fields(self.type_of(typ)).unwrap();
                            field_bits
                                .iter()
                                .zip(fields.iter())
                                .all(|(bits, field)| {
                                    self.valid(self.type_id(&field.param_types[0]), bits)
                                })
                        }
                        None => false,
                    }
                }
                Some(TypeLayoutKind::Other(_)) => true,
                _ => self.decode_ctor(typ, bits).is_some(),
            },
        }
    }
}

/// Build the bit-layout map for every user-defined type
/// instantiation reachable from `seeds`, interning every type
/// encountered (seed types, layout keys, and member types) into
/// `types`.  Returns the bare layout map; wrap it with
/// [`TypeLayoutIndex::from_parts`] once any remaining expression
/// types have been interned (the `sim` flow does this after
/// expression conversion).
///
/// This is the shared construction core; the simulator seeds it from
/// its elaborated component types (`sim::layout::build_layouts`), and
/// type-only tools seed it from the types they care about
/// ([`TypeLayoutIndex::for_types`]).
pub(crate) fn build_layouts(
    db: &Db,
    seeds: &[Type],
    types: &mut TypeInterner,
) -> IndexMap<Type, TypeLayout> {
    types.intern(&Type::Bit);
    types.intern(&Type::Clock);
    types.intern(&Type::Reset);

    let mut layouts: IndexMap<Type, TypeLayout> = IndexMap::new();
    let mut queue: Vec<Type> = Vec::new();

    for typ in seeds {
        intern_all(typ, types);
        seed_type(typ, &mut queue);
    }

    let symboltable = db.get_symboltable();

    while let Some(typ) = queue.pop() {
        if layouts.contains_key(&typ) {
            continue;
        }
        intern_all(&typ, types);
        let layout = build_type_layout(db, &symboltable, &typ, &mut queue);
        layouts.insert(typ, layout);
    }

    // Pre-intern every `Word[w]` up to the widest type reachable from
    // the seeds (and at least up to 128, for literal handling): tools
    // that build new values at runtime — slices, arithmetic results,
    // literal coercions, e.g. in the `vir poke` tester — need a
    // `TypeId` for widths that may not appear as any seed's type.
    let max_width = seeds
        .iter()
        .map(|typ| layout_width(&layouts, typ))
        .max()
        .unwrap_or(0)
        .max(128);
    for width in 0..=max_width {
        types.intern(&Type::Word(width));
    }

    // Member types (union/enum slot parameters, struct fields) can
    // be primitive (`Word`/`Bit`), which `seed_type` does not seed;
    // intern them so runtime value construction (e.g. `match` arm
    // bindings) can always resolve a `TypeId`.
    for layout in layouts.values() {
        let member_types: &[Slot] = match &layout.kind {
            TypeLayoutKind::Union { slots } => slots,
            TypeLayoutKind::Enum { slots, .. } => slots,
            TypeLayoutKind::Struct { fields } => fields,
            TypeLayoutKind::Other(_) => continue,
        };
        for slot in member_types {
            for typ in &slot.param_types {
                intern_all(typ, types);
            }
        }
    }

    layouts
}

/// Build the `TypeLayout` for the type instantiation `typ`
/// (`Type::Usual(sym, args)`), seeding `queue` with any user-defined
/// type instantiations found in its (substituted) member types.
fn build_type_layout(
    db: &Db,
    symboltable: &crate::analysis::symbols::SymbolTable,
    typ: &Type,
    queue: &mut Vec<Type>,
) -> TypeLayout {
    let Type::Usual(sym, args) = typ else {
        return TypeLayout { name: BString::from(""), kind: TypeLayoutKind::Other(0) };
    };
    let typedef: Arc<TypeDef> = db.get_typedef(*sym);
    let name = symboltable.symbol(*sym).name().to_owned();
    let slots = symboltable.slots(*sym);
    let slot_of = |slot_sym: &&crate::analysis::symbols::Symbol| -> Slot {
        let sig = db.get_ctor_signature(slot_sym.id());
        let param_types: Vec<Type> = sig
            .parameters
            .iter()
            .map(|(_name, t)| {
                let substituted = substitute(t, args);
                seed_type(&substituted, queue);
                substituted
            })
            .collect();
        Slot {
            symbol_id: slot_sym.id(),
            name: slot_sym.name().to_owned(),
            param_types,
        }
    };
    let kind = match typedef.kind {
        TypeScheme::UnionDef => {
            TypeLayoutKind::Union { slots: slots.iter().map(slot_of).collect() }
        }
        TypeScheme::StructDef => {
            let fields: Vec<Slot> = db
                .get_struct_fields(*sym)
                .into_iter()
                .filter_map(|f| {
                    f.typ.clone().map(|t| {
                        let substituted = substitute(&t, args);
                        seed_type(&substituted, queue);
                        Slot {
                            symbol_id: f.symbol_id(),
                            name: f.name.clone(),
                            param_types: vec![substituted],
                        }
                    })
                })
                .collect();
            TypeLayoutKind::Struct { fields }
        }
        TypeScheme::EnumDef => {
            let tag_width =
                typedef.width.unwrap_or_else(|| slots.len().try_into().unwrap());
            TypeLayoutKind::Enum {
                tag_width,
                enumerant_values: typedef.enumerant_values.clone(),
                slots: slots.iter().map(slot_of).collect(),
            }
        }
        _ => {
            let width = typedef.width.unwrap_or_else(|| slots.len().try_into().unwrap());
            TypeLayoutKind::Other(width)
        }
    };
    TypeLayout { name, kind }
}

/// Push any user-defined type instantiation embedded in `typ` onto
/// `queue` for processing, recursing through `Usual` argument lists.
fn seed_type(typ: &Type, queue: &mut Vec<Type>) {
    if let Type::Usual(_, args) = typ {
        queue.push(typ.clone());
        for arg in args {
            seed_type(arg, queue);
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::symbols::SymbolKind;
    use crate::common::source::Source;
    use crate::db::Db;
    use crate::package::PackageTable;
    use bstr::BString;

    /// A small project exercising every layout shape: struct, enum,
    /// union, generics, and nesting.
    fn db_with_project() -> Db {
        let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let builtin = std::fs::read_to_string(root.join("../lib/builtin.vir")).unwrap();
        let scratch = r#"
            pub struct type Pair[T : Type, U : Type] {
                first : T
                second : U
            }

            pub enum type State {
                Idle
                Run(count : Word[8])
            }

            pub union type Cell[T : Type] {
                Empty
                Full(value : T)
            }

            pub mod Top {
                incoming clock : Clock
                incoming inp : Word[4]

                outgoing a : Pair[Word[4], Bit]
                outgoing b : State
                outgoing c : Cell[Word[4]]

                a := ${first = inp, second = true}
                b := @Run(inp)
                c := @Full(inp)
            }
        "#;
        let mut db = Db::new();
        let names: Vec<BString> = vec![BString::from("builtin"), BString::from("scratch")];
        db.set_packages(PackageTable::new(names));
        let packages = db.get_packages();
        for (name, text) in [("builtin", builtin.as_str()), ("scratch", scratch)] {
            let package = packages.id(BString::from(name).as_bstr()).unwrap();
            db.set_source(package, Source::new(package, text.to_string().into()));
        }
        db
    }

    /// The standalone entry point reproduces exactly what the
    /// simulator's seeding (`sim::layout::build_layouts`, which calls
    /// this same core with elaboration component types) computes:
    /// same layout map, same intern table contents.
    #[test]
    fn standalone_matches_sim_seeded_layouts() {
        let db = db_with_project();

        let symboltable = db.get_symboltable();
        let top = symboltable
            .items()
            .into_iter()
            .find(|sym| sym.kind == SymbolKind::ModDef && sym.name().as_bstr() == "Top")
            .unwrap();
        let elab = db.get_elaboration(top.id());

        // The simulator's seeding: elaborated component types.
        let seeds: Vec<Type> = elab
            .components()
            .iter()
            .map(|comp| comp.typ().clone())
            .collect();

        let mut sim_interner = TypeInterner::new();
        let sim_layouts = build_layouts(&db, &seeds, &mut sim_interner);
        let sim_index = TypeLayoutIndex::from_parts(sim_layouts, sim_interner);

        let standalone = TypeLayoutIndex::for_types(&db, seeds.clone());

        // Every seeded type gets the same width from both indexes
        // (widths exercise the whole layout recursively).
        for typ in &seeds {
            assert_eq!(
                sim_index.type_width(typ),
                standalone.type_width(typ),
                "width mismatch for {typ:?}"
            );
        }
    }

    /// The `for_types` entry point reproduces the sim-built layout for
    /// a type seeded through an elaboration.
    #[test]
    fn for_types_reproduces_component_layouts() {
        let db = db_with_project();
        let symboltable = db.get_symboltable();
        let top = symboltable
            .items()
            .into_iter()
            .find(|sym| sym.kind == SymbolKind::ModDef && sym.name().as_bstr() == "Top")
            .unwrap();
        let elab = db.get_elaboration(top.id());

        let seeds: Vec<Type> = elab
            .components()
            .iter()
            .map(|comp| comp.typ().clone())
            .collect();
        let standalone = TypeLayoutIndex::for_types(&db, seeds.clone());

        for typ in &seeds {
            let via_sim = {
                let mut interner = TypeInterner::new();
                let layouts = build_layouts(&db, &seeds, &mut interner);
                let index = TypeLayoutIndex::from_parts(layouts, interner);
                index.type_width(typ)
            };
            assert_eq!(standalone.type_width(typ), via_sim, "width mismatch for {typ:?}");
        }

        // Spot-check the documented layout rules through the
        // standalone index: `Valid[Word[8]]` is 10 bits (2-bit one-hot
        // tag + 8-bit payload) and `@Valid(0)` puts the one-hot tag in
        // the low bits.
        let valid_sym = symboltable
            .typedefs()
            .into_iter()
            .find(|sym| sym.kind() == SymbolKind::EnumDef && sym.name().as_bstr() == "Valid")
            .unwrap()
            .id();
        let valid = Type::Usual(valid_sym, vec![Type::Word(8)]);
        let index = TypeLayoutIndex::for_types(&db, vec![valid.clone()]);
        assert_eq!(index.type_width(&valid), 10);
    }
}
