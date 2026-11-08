//! Bit-layout for the simulator — now a thin shim over the shared
//! implementation in [`crate::typelayout`].
//!
//! The layout *rules* (one-hot union tags, enum tags, struct field
//! concatenation, validity) live in exactly one place,
//! `crate::typelayout`; this module only supplies the simulator's
//! seeding: the same construction core run over the elaborated
//! component types.

use indexmap::IndexMap;

use crate::analysis::elaboration::Elaboration;
use crate::analysis::symbols::SymbolId;
use crate::common::Width;
use crate::db::Db;
use crate::types::typ::Type;
use crate::typelayout::TypeLayout;

pub use crate::typelayout::{TypeId, TypeInterner, TypeLayoutIndex};

/// Compile-time geometry of one constructor of a union/enum type:
/// how `encode_ctor` lays the tag and parameters into the type's
/// bits.  Pure data derived from the layout tables, the compiled
/// simulator's lowering consumes it instead of calling
/// `encode_ctor`/`decode_ctor` per evaluation.
#[derive(Clone, Debug)]
pub(super) struct CtorGeometry {
    /// Total bit width of the encoded type.
    pub total_width: Width,
    /// Width of the tag field, bits `[0, tag_width)`.
    pub tag_width: Width,
    /// The tag value this constructor encodes (one-hot index for
    /// unions, enumerant value for enums).
    pub tag: u64,
    /// Per-parameter payload bit ranges `(lo, hi)` (`hi` exclusive,
    /// MSB-first Virdant range convention), in parameter
    /// declaration order.  The first parameter sits at the MSB end
    /// of the payload area: parameter `k` occupies bits
    /// `[tag_width + sum_{j>k} w_j, tag_width + sum_{j>=k} w_j)`.
    /// Consumed by the `match` lowering's `Extract` emission.
    pub params: Vec<(Width, Width)>,
    /// The interned type of each parameter, in the same declaration
    /// order as `params`.  The nested-pattern `match` lowering
    /// recurses through it: a nested `Ctor` sub-pattern decodes its
    /// payload slot through `ctor_geometry(param_typ, ..)`.
    pub param_typs: Vec<TypeId>,
}

impl TypeLayoutIndex {
    /// Pure-geometry accessor for the compiled-sim lowering: how
    /// constructor `ctor_sym` of `typ` lays out into bits.  Returns
    /// `None` when `typ` is not a union/enum or has no constructor
    /// with that symbol.
    ///
    /// Caveat carried over from `decode_ctor` (which the match
    /// lowering must reproduce): a *valid tag* whose payload padding
    /// bits, between `tag_width + payload_width` and `total_width` —
    /// are nonzero makes `decode_ctor` return `None`, so today a
    /// match arm is NOT taken for such a pattern even though the tag
    /// itself matches.  Lowering that bypasses `decode_ctor` must
    /// replicate this padding-validity check or change match-arm
    /// selection behavior.
    pub(super) fn ctor_geometry(&self, typ: TypeId, ctor_sym: SymbolId) -> Option<CtorGeometry> {
        let layout = self.layout_of(typ)?;
        let (tag_width, tag) = match &layout.kind {
            crate::typelayout::TypeLayoutKind::Union { slots } => {
                let index = slots.iter().position(|s| s.symbol_id == ctor_sym)?;
                (slots.len().try_into().unwrap(), 1u64 << index)
            }
            crate::typelayout::TypeLayoutKind::Enum { tag_width, enumerant_values, .. } => {
                (*tag_width, *enumerant_values.get(&ctor_sym)?)
            }
            _ => return None,
        };
        let total_width = self.type_width(self.type_of(typ));
        let slot_index = match &layout.kind {
            crate::typelayout::TypeLayoutKind::Union { slots } => slots.iter().position(|s| s.symbol_id == ctor_sym)?,
            crate::typelayout::TypeLayoutKind::Enum { slots, .. } => slots.iter().position(|s| s.symbol_id == ctor_sym)?,
            _ => unreachable!(),
        };
        let slot = match &layout.kind {
            crate::typelayout::TypeLayoutKind::Union { slots } => &slots[slot_index],
            crate::typelayout::TypeLayoutKind::Enum { slots, .. } => &slots[slot_index],
            _ => unreachable!(),
        };
        let payload_width: Width = slot.param_types.iter().map(|t| self.type_width(t)).sum();
        let mut params = Vec::with_capacity(slot.param_types.len());
        let mut param_typs = Vec::with_capacity(slot.param_types.len());
        let mut hi = tag_width + payload_width;
        for param_type in &slot.param_types {
            let w = self.type_width(param_type);
            params.push((hi - w, hi));
            param_typs.push(self.type_id(param_type));
            hi -= w;
        }
        Some(CtorGeometry { total_width, tag_width, tag, params, param_typs })
    }

    /// Build the bit-layout map for every user-defined type
    /// instantiation reachable from the elaborated component types,
    /// interning every type encountered (component types, layout keys,
    /// and member types) into `types`.  Returns the bare layout map;
    /// wrap it with [`TypeLayoutIndex::from_parts`] once expression
    /// conversion has finished interning the expression types.
    pub(super) fn build_layouts(
        db: &Db,
        elab: &Elaboration,
        types: &mut TypeInterner,
    ) -> IndexMap<Type, TypeLayout> {
        let seeds: Vec<Type> = elab
            .components()
            .iter()
            .map(|comp| comp.typ().clone())
            .collect();
        crate::typelayout::build_layouts(db, &seeds, types)
    }
}
