//! The single shared bit-width computation for `Type`, recursing
//! through `Type::Usual` argument lists and substituting generic
//! members before measuring them.  Usable both from a plain `Db`
//! (verilog backend, vcd dumper) and from a `Builder` (typechecker).

use std::sync::Arc;

use crate::analysis::structs::StructField;
use crate::analysis::symbols::{SymbolId, SymbolTable};
use crate::common::Width;
use crate::types::signature::Signature;
use crate::types::substitute::substitute;
use crate::types::typ::Type;
use crate::types::typedef::TypeDef;

/// The database context `type_width` needs.  Implemented for `&Db` and
/// for `Builder`, so the width computation is shared by every
/// consumer instead of hand-rolled per backend.
pub(crate) trait WidthCtx {
    fn symboltable(&mut self) -> Arc<SymbolTable>;
    fn typedef(&mut self, symbol_id: SymbolId) -> Arc<TypeDef>;
    fn ctor_signature(&mut self, ctor_symbol_id: SymbolId) -> Arc<Signature>;
    fn struct_fields(&mut self, symbol_id: SymbolId) -> Vec<StructField>;
}

impl<'d> WidthCtx for &'d crate::db::Db {
    fn symboltable(&mut self) -> Arc<SymbolTable> {
        self.get_symboltable()
    }

    fn typedef(&mut self, symbol_id: SymbolId) -> Arc<TypeDef> {
        self.get_typedef(symbol_id)
    }

    fn ctor_signature(&mut self, ctor_symbol_id: SymbolId) -> Arc<Signature> {
        self.get_ctor_signature(ctor_symbol_id)
    }

    fn struct_fields(&mut self, symbol_id: SymbolId) -> Vec<StructField> {
        self.get_struct_fields(symbol_id)
    }
}

impl<'d> WidthCtx for crate::db::Builder<'d> {
    fn symboltable(&mut self) -> Arc<SymbolTable> {
        self.get_symboltable()
    }

    fn typedef(&mut self, symbol_id: SymbolId) -> Arc<TypeDef> {
        self.get_typedef(symbol_id)
    }

    fn ctor_signature(&mut self, ctor_symbol_id: SymbolId) -> Arc<Signature> {
        self.get_ctor_signature(ctor_symbol_id)
    }

    fn struct_fields(&mut self, symbol_id: SymbolId) -> Vec<StructField> {
        self.get_struct_fields(symbol_id)
    }
}

/// The bit width of the mono type `typ`.
///
/// Debug-asserts that `typ` contains no `Param`: widths are a mono
/// artifact, and a generic type reaching this point is a compiler bug.
pub(crate) fn type_width<C: WidthCtx>(ctx: &mut C, typ: &Type) -> Width {
    debug_assert!(typ.is_mono(), "type_width on a non-mono type: {typ:?}");
    match typ {
        Type::Bit | Type::Clock | Type::Reset => 1,
        Type::Word(w) => *w,
        Type::Param(_) => 0,
        Type::Usual(symbol_id, args) => usual_width(ctx, *symbol_id, args),
    }
}

/// The width of a user-defined type instantiated at `args`.
///
/// * union: one-hot tag (one bit per ctor) + widest payload
/// * struct: sum of the field widths
/// * enum: the tag width (the enum's declared width) + widest payload,
///   which is 0 for enums whose enumerants carry no parameters
pub(crate) fn usual_width<C: WidthCtx>(ctx: &mut C, symbol_id: SymbolId, args: &[Type]) -> Width {
    let typedef = ctx.typedef(symbol_id);
    match typedef.kind {
        crate::common::TypeScheme::UnionDef => {
            let slots = slots_of(ctx, symbol_id);
            let tag_width: Width = slots.len().try_into().unwrap_or(0);
            let max_payload: Width = slots
                .iter()
                .map(|slot| slot_payload_width(ctx, *slot, args))
                .max()
                .unwrap_or(0);
            tag_width + max_payload
        }
        crate::common::TypeScheme::StructDef => {
            let fields = ctx.struct_fields(symbol_id);
            fields
                .iter()
                .map(|field| match &field.typ {
                    Some(typ) => type_width(ctx, &substitute(typ, args)),
                    None => 0,
                })
                .sum()
        }
        crate::common::TypeScheme::EnumDef => {
            let tag_width = enum_tag_width(ctx, symbol_id, &typedef);
            let slots = slots_of(ctx, symbol_id);
            let max_payload: Width = slots
                .iter()
                .map(|slot| slot_payload_width(ctx, *slot, args))
                .max()
                .unwrap_or(0);
            tag_width + max_payload
        }
        _ => {
            let slots = slots_of(ctx, symbol_id);
            typedef.width.unwrap_or_else(|| slots.len().try_into().unwrap_or(0))
        }
    }
}

/// The width of the tag of an enum type: the enum's declared width if
/// given, else one bit per enumerant (the one-hot default).
pub(crate) fn enum_tag_width<C: WidthCtx>(ctx: &mut C, symbol_id: SymbolId, typedef: &TypeDef) -> Width {
    if let Some(width) = typedef.width {
        width
    } else {
        slots_of(ctx, symbol_id).len().try_into().unwrap_or(0)
    }
}

fn slots_of<C: WidthCtx>(ctx: &mut C, symbol_id: SymbolId) -> Vec<SymbolId> {
    let symboltable = ctx.symboltable();
    symboltable
        .slots(symbol_id)
        .iter()
        .map(|slot| slot.id())
        .collect()
}

/// The summed width of one ctor's (or payload enumerant's) parameter
/// types, with the def's generic parameters substituted by `args`.
pub(crate) fn slot_payload_width<C: WidthCtx>(ctx: &mut C, slot: SymbolId, args: &[Type]) -> Width {
    let sig = ctx.ctor_signature(slot);
    sig.parameters
        .iter()
        .map(|(_name, typ)| type_width(ctx, &substitute(typ, args)))
        .sum()
}
