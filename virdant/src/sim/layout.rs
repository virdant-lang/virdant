//! Bit-layout for Virdant's type system, as used by the simulator.
//!
//! Every simulated value is a bit pattern (see `sim::bits`); this module
//! defines how each type's inhabitants map onto bits:
//!
//! - `Bit` / `Clock` / `Reset` are 1 bit.
//! - `Word[w]` is `w` bits.
//! - A struct is the concatenation of its fields' bits, first field at
//!   the MSB end, filling the width exactly.
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

use std::sync::Arc;

use bstr::{BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::elaboration::Elaboration;
use crate::analysis::symbols::SymbolId;
use crate::common::Width;
use crate::db::Db;
use crate::sim::bits::BitPat;
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


#[derive(Clone, Debug)]
enum TypeLayoutKind {
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
struct TypeLayout {
    /// The typedef's (simple) name, for display.
    #[allow(dead_code)]
    name: BString,
    kind: TypeLayoutKind,
}

/// Index of precomputed bit-layout data for every user-defined type
/// instantiation reachable from the elaborated component types.  Built
/// once from `&Db`; cloned cheaply (inner map behind an `Arc`).
#[derive(Clone, Debug)]
pub struct TypeLayoutIndex {
    layouts: Arc<IndexMap<Type, TypeLayout>>,
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

    /// Build a `TypeLayoutIndex` covering every user-defined type
    /// instantiation reachable from the elaborated component types.
    pub(super) fn build(db: &Db, elab: &Elaboration) -> Self {
        let mut layouts: IndexMap<Type, TypeLayout> = IndexMap::new();
        let mut queue: Vec<Type> = Vec::new();

        for comp in elab.components() {
            seed_type(&comp.typ(), &mut queue);
        }

        let symboltable = db.get_symboltable();

        while let Some(typ) = queue.pop() {
            if layouts.contains_key(&typ) {
                continue;
            }
            let layout = build_type_layout(db, &symboltable, &typ, &mut queue);
            layouts.insert(typ, layout);
        }

        TypeLayoutIndex { layouts: Arc::new(layouts) }
    }

    fn layout(&self, typ: &Type) -> Option<&TypeLayout> {
        self.layouts.get(typ)
    }

    /// The bit width of a value of `typ`.  `Type::Param` is generic and
    /// never appears in mono artifacts; it yields 0.
    pub fn type_width(&self, typ: &Type) -> Width {
        match typ {
            Type::Bit | Type::Clock | Type::Reset => 1,
            Type::Word(w) => *w,
            Type::Param(_) => 0,
            Type::Usual(..) => match self.layout(typ).map(|l| &l.kind) {
                Some(TypeLayoutKind::Union { slots }) => {
                    let tag_width: Width = slots.len().try_into().unwrap();
                    tag_width + self.max_payload_width(slots)
                }
                Some(TypeLayoutKind::Enum { tag_width, slots, .. }) => {
                    *tag_width + self.max_payload_width(slots)
                }
                Some(TypeLayoutKind::Struct { fields }) => fields
                    .iter()
                    .map(|f| self.type_width(&f.param_types[0]))
                    .sum(),
                Some(TypeLayoutKind::Other(width)) => *width,
                None => 0,
            },
        }
    }

    fn max_payload_width(&self, slots: &[Slot]) -> Width {
        slots
            .iter()
            .map(|s| s.param_types.iter().map(|t| self.type_width(t)).sum::<Width>())
            .max()
            .unwrap_or(0)
    }

    /// The tag width for a union/enum type.
    fn tag_width(&self, typ: &Type) -> Option<Width> {
        match self.layout(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Union { slots }) => Some(slots.len().try_into().unwrap()),
            Some(TypeLayoutKind::Enum { tag_width, .. }) => Some(*tag_width),
            _ => None,
        }
    }

    /// The slots of a union/enum type (in tag order).
    pub(crate) fn slots(&self, typ: &Type) -> Option<&[Slot]> {
        match self.layout(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Union { slots }) => Some(slots),
            Some(TypeLayoutKind::Enum { slots, .. }) => Some(slots),
            _ => None,
        }
    }

    /// The fields of a struct type, in declaration order.
    #[allow(dead_code)]
    pub(crate) fn fields(&self, typ: &Type) -> Option<&[Slot]> {
        match self.layout(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Struct { fields }) => Some(fields),
            _ => None,
        }
    }

    /// The tag value selecting `ctor_sym` in a union/enum type.
    pub(crate) fn ctor_tag_value(&self, typ: &Type, ctor_sym: SymbolId) -> Option<u64> {
        match self.layout(typ).map(|l| &l.kind) {
            Some(TypeLayoutKind::Union { slots }) => {
                let index = slots.iter().position(|s| s.symbol_id == ctor_sym)?;
                Some(1u64 << index)
            }
            Some(TypeLayoutKind::Enum { enumerant_values, .. }) => {
                Some(*enumerant_values.get(&ctor_sym)?)
            }
            _ => None,
        }
    }

    /// Encode a constructor application as a bit pattern of `typ`:
    /// the tag in the low bits, the encoded parameters above (first
    /// parameter at the MSB end), zero padding up to the type width.
    pub(crate) fn encode_ctor(
        &self,
        typ: &Type,
        ctor_sym: SymbolId,
        args: &[BitPat],
    ) -> Option<BitPat> {
        let tag_width = self.tag_width(typ)?;
        let tag = self.ctor_tag_value(typ, ctor_sym)?;
        let total = self.type_width(typ);

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
    pub(crate) fn decode_ctor(&self, typ: &Type, bits: &BitPat) -> Option<(Slot, Vec<BitPat>)> {
        let slots: Vec<Slot> = self.slots(typ)?.to_vec();
        let tag_width = self.tag_width(typ)?;
        let tag = bits.slice(0, tag_width).to_u64();

        let slot_index = slots
            .iter()
            .position(|s| self.ctor_tag_value(typ, s.symbol_id) == Some(tag))?;
        let slot = slots[slot_index].clone();

        // Payload area: bits [tag_width, tag_width + payload_width), zero
        // padding above.
        let payload_width: Width = slot.param_types.iter().map(|t| self.type_width(t)).sum();
        let total = self.type_width(typ);
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
        Some((slot, payloads))
    }

    /// Decode a bit pattern of a struct type into its fields' bit
    /// patterns, in declaration order (first field at the MSB end).
    /// Returns `None` when `typ` is not a struct (or the pattern is the
    /// wrong width).
    #[allow(dead_code)]
    pub(crate) fn decode_struct(&self, typ: &Type, bits: &BitPat) -> Option<Vec<BitPat>> {
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

    /// True when `bits` is a valid inhabitant of `typ`.
    #[allow(dead_code)]
    pub(crate) fn valid(&self, typ: &Type, bits: &BitPat) -> bool {
        match typ {
            Type::Bit | Type::Clock | Type::Reset | Type::Word(_) => true,
            Type::Param(_) => false,
            Type::Usual(..) => match self.layout(typ).map(|l| &l.kind) {
                Some(TypeLayoutKind::Struct { .. }) => match self.decode_struct(typ, bits) {
                    Some(field_bits) => {
                        let fields = self.fields(typ).unwrap();
                        field_bits
                            .iter()
                            .zip(fields.iter())
                            .all(|(bits, field)| {
                                self.valid(&field.param_types[0], bits)
                            })
                    }
                    None => false,
                },
                Some(TypeLayoutKind::Other(_)) => true,
                _ => self.decode_ctor(typ, bits).is_some(),
            },
        }
    }

    /// Render `bits` (a pattern of `typ`) as a user-facing string,
    /// according to the object type.  Invalid patterns print as
    /// `<invalid>`.
    #[allow(dead_code)]
    pub(crate) fn display(&self, typ: &Type, bits: &BitPat) -> String {
        if !self.valid(typ, bits) {
            return "<invalid>".into();
        }
        match typ {
            Type::Bit => if bits.get_bit(0) { "true".into() } else { "false".into() },
            Type::Clock | Type::Reset => bits.to_bit_string(),
            Type::Word(w) => {
                if *w < 64 {
                    bits.to_u64().to_string()
                } else {
                    format!("0b{}", bits.to_bit_string())
                }
            }
            Type::Usual(..) => match self.layout(typ).map(|l| &l.kind) {
                Some(TypeLayoutKind::Struct { .. }) => {
                    let fields = self.fields(typ).unwrap().to_vec();
                    let field_bits = self.decode_struct(typ, bits).unwrap();
                    let args: Vec<String> = fields
                        .iter()
                        .zip(field_bits.iter())
                        .map(|(field, bits)| {
                            format!(
                                "{}={}",
                                field.name.as_bstr(),
                                self.display(&field.param_types[0], bits)
                            )
                        })
                        .collect();
                    format!("{}{{{}}}", self.layout(typ).unwrap().name.as_bstr(), args.join(", "))
                }
                kind => {
                    let (slot, payloads) = self.decode_ctor(typ, bits).unwrap();
                    let args: Vec<String> = payloads
                        .iter()
                        .zip(slot.param_types.iter())
                        .map(|(bits, typ)| self.display(typ, bits))
                        .collect();
                    match kind {
                        Some(TypeLayoutKind::Enum { .. }) => format!("#{}", slot.name.as_bstr()),
                        _ => {
                            if args.is_empty() {
                                format!("#{}", slot.name.as_bstr())
                            } else {
                                format!("#{}({})", slot.name.as_bstr(), args.join(", "))
                            }
                        }
                    }
                }
            },
            Type::Param(_) => "<invalid>".into(),
        }
    }
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
