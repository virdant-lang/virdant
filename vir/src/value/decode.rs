//! Value decoding for `vir value decode`: turn a bit pattern into a
//! value, type-directed by the layout — the inverse of [`crate::encode`].
//!
//! The pattern must be exactly as wide as the type's layout (no
//! implicit zero-extension: a shorter pattern would silently
//! mis-decode padded union/enum inhabitants).  After the width check,
//! the tag is validated (precise `unknown tag` / `non-zero padding`
//! messages), then the whole pattern is validated recursively
//! ([`TypeLayoutIndex::valid`]) before rendering.

use bstr::ByteSlice;

use virdant::sim::BitPat;
use virdant::typelayout::{TypeLayoutIndex, TypeLayoutKind};
use virdant::types::typ::Type;

use crate::encode::{bitpat_u128, type_display};
use crate::resolve::Resolver;

/// Decode `bits` as a value of `typ` and render it in round-trippable
/// syntax.
pub fn decode(
    resolver: &Resolver,
    layouts: &TypeLayoutIndex,
    typ: &Type,
    bits: &BitPat,
) -> Result<String, String> {
    let total = layouts.type_width(typ);
    if bits.width() != total {
        return Err(format!(
            "pattern is {} bit{} wide, but type {} is {} bit{} wide",
            bits.width(),
            if bits.width() == 1 { "" } else { "s" },
            type_display(resolver, layouts, typ),
            total,
            if total == 1 { "" } else { "s" },
        ));
    }

    if let Type::Usual(..) = typ {
        if layouts.valid(layouts.type_id(typ), bits) {
            return Ok(render(layouts, typ, bits));
        }
        // Not a valid inhabitant: produce a precise message.
        Err(invalid_reason(resolver, layouts, typ, bits))
    } else {
        Ok(render(layouts, typ, bits))
    }
}

/// Diagnose why `bits` is not a valid inhabitant of `typ` (which is a
/// user-defined type).  Checks the top-level tag first (union one-hot,
/// enum enumerant value), then padding, then recurses into payloads.
fn invalid_reason(
    resolver: &Resolver,
    layouts: &TypeLayoutIndex,
    typ: &Type,
    bits: &BitPat,
) -> String {
    let total = layouts.type_width(typ);
    let type_name = type_display(resolver, layouts, typ);

    let Some(slots) = layouts.slots_of(typ) else {
        return format!("pattern is not a valid inhabitant of {type_name}");
    };

    // Tag area.
    let tag_width: u16 = match layouts.kind_of(typ) {
        Some(TypeLayoutKind::Union { .. }) => slots.len() as u16,
        Some(TypeLayoutKind::Enum { tag_width, .. }) => *tag_width,
        _ => return format!("pattern is not a valid inhabitant of {type_name}"),
    };
    let tag_bits = bits.slice(0, tag_width);
    let tag = tag_bits.to_u64();

    // Find the selected slot (if any) the same way `decode_ctor` does.
    let selected: Option<usize> = match layouts.kind_of(typ) {
        Some(TypeLayoutKind::Enum { .. }) => {
            let values = layouts.enumerant_values_of(typ).unwrap();
            match slots
                .iter()
                .position(|s| values.get(&s.symbol_id).copied() == Some(tag))
            {
                Some(i) => Some(i),
                None => {
                    return format!(
                        "unknown tag {tag} for {type_name}: not a known enumerant value"
                    );
                }
            }
        }
        _ => {
            // Union: one-hot in the low tag bits.
            if tag == 0 || (tag & (tag - 1)) != 0 {
                return format!(
                    "unknown tag 0b{} for {type_name}: union tags are one-hot in the low {tag_width} bits",
                    tag_bits.to_bit_string()
                );
            }
            let index = tag.trailing_zeros() as usize;
            if index >= slots.len() {
                return format!(
                    "unknown tag 0b{} for {type_name}: tag bit {index} selects no slot",
                    tag_bits.to_bit_string()
                );
            }
            Some(index)
        }
    };

    // Padding: bits above the selected slot's payload must be zero.
    let index = selected.unwrap();
    let slot = &slots[index];
    let payload_width: u16 = slot.param_types.iter().map(|t| layouts.type_width(t)).sum();
    if total > tag_width + payload_width {
        let padding = bits.slice(tag_width + payload_width, total);
        if !padding.is_zero() {
            return format!(
                "non-zero padding above the payload of slot '{}' (bits [{}, {})): 0b{}",
                slot.name.to_str_lossy(),
                tag_width + payload_width,
                total,
                padding.to_bit_string()
            );
        }
    }

    // Recurse into the payload: find the first invalid member.
    let mut payloads = Vec::with_capacity(slot.param_types.len());
    let mut hi = tag_width + payload_width;
    for param_type in &slot.param_types {
        let w = layouts.type_width(param_type);
        payloads.push(bits.slice(hi - w, hi));
        hi -= w;
    }
    for (param_type, pat) in slot.param_types.iter().zip(payloads.iter()) {
        if let Type::Usual(..) = param_type {
            if !layouts.valid(layouts.type_id(param_type), pat) {
                return format!(
                    "payload is not a valid inhabitant of {}: {}",
                    type_display(resolver, layouts, param_type),
                    invalid_reason(resolver, layouts, param_type, pat)
                );
            }
        }
    }

    format!("pattern is not a valid inhabitant of {type_name}")
}

/// Render `bits` (a valid inhabitant of `typ`) in value syntax:
///
/// - `Word[N]` as a plain decimal integer (width comes from context;
///   the `wN` suffix is stripped for now);
/// - `Bit` as `true`/`false`;
/// - `Clock`/`Reset` as full-width binary (`0b0`);
/// - enums and unions as constructor syntax, recursively:
///   `@Valid(0)`, `@Invalid()`;
/// - structs as `${Name(f = value, ...)}`.
pub fn render(layouts: &TypeLayoutIndex, typ: &Type, bits: &BitPat) -> String {
    match typ {
        Type::Bit => {
            if bits.get_bit(0) { "true".to_string() } else { "false".to_string() }
        }
        Type::Clock | Type::Reset => format!("0b{}", bits.to_bit_string()),
        Type::Word(_) => format!("{}", to_u128(bits)),
        Type::Param(_) => "<invalid>".to_string(),
        Type::Usual(..) => {
            if let Some(fields) = layouts.fields(typ) {
                let field_bits = layouts.decode_struct(typ, bits).unwrap_or_default();
                let rendered: Vec<String> = fields
                    .iter()
                    .zip(field_bits.iter())
                    .map(|(field, pat)| {
                        format!(
                            "{} = {}",
                            field.name.to_str_lossy(),
                            render(layouts, &field.param_types[0], pat)
                        )
                    })
                    .collect();
                let name = layouts
                    .type_name(typ)
                    .map(|n| n.to_str_lossy().into_owned())
                    .unwrap_or_default();
                format!("${{{name}({})}}", rendered.join(", "))
            } else if let Some((slot, payloads)) =
                layouts.decode_ctor(layouts.type_id(typ), bits)
            {
                // Constructors ALWAYS print with parens — `@Invalid()`
                // even with no payload — so every decode output is a
                // valid round-trippable value expression.
                let args: Vec<String> = payloads
                    .iter()
                    .zip(slot.param_types.iter())
                    .map(|(pat, t)| render(layouts, t, pat))
                    .collect();
                format!("@{}({})", slot.name.to_str_lossy(), args.join(", "))
            } else {
                // Unreachable: callers validate before rendering.
                "<invalid>".to_string()
            }
        }
    }
}

/// The `to_u128` of a bit pattern (beyond the 64-bit fast path).
fn to_u128(bits: &BitPat) -> u128 {
    let w = bits.width();
    if w <= 64 {
        bits.to_u64() as u128
    } else {
        let mut out = 0u128;
        for i in 0..w.min(128) as usize {
            if bits.get_bit(i) {
                out |= 1u128 << i;
            }
        }
        out
    }
}

/// Build the pattern for a `decode` input literal, given the decoded
/// type's width.
pub fn pattern_bits(pattern: &crate::ast::PatternLit, width: u16) -> Result<BitPat, String> {
    match pattern {
        crate::ast::PatternLit::Nat(v) => {
            if width < 128 && *v >= (1u128 << width) {
                return Err(format!("pattern {v} does not fit in the {width}-bit type"));
            }
            Ok(bitpat_u128(width, *v))
        }
        crate::ast::PatternLit::Word(v, w) => {
            if *w != width {
                return Err(format!(
                    "pattern literal is {w} bits wide, but the type is {width} bits wide"
                ));
            }
            if *w < 128 && *v >= (1u128 << *w) {
                return Err(format!("pattern value {v} does not fit in {w} bits"));
            }
            Ok(bitpat_u128(width, *v))
        }
    }
}
