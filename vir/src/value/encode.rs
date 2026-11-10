//! Value encoding for `vir value encode`: turn a value expression
//! into a bit pattern, type-directed by the layout.
//!
//! Two passes (see PLAN §4):
//!
//! 1. **Infer** the top-level type of the expression, when possible.
//!    Word literals (explicit `w` width), `Bit` literals, ascriptions,
//!    and named struct literals determine their own types; constructor
//!    applications determine their type by resolving the constructor
//!    (by simple name, across every package) and unifying each
//!    payload's inferred type against the ctor's written parameter
//!    types.  Anything left undetermined (a bare `Nat` literal, an
//!    uninferable generic parameter) makes inference fail — with no
//!    expected type, that is the `Can't infer type` error.
//!
//! 2. **Encode** against the expected type: literals take their width
//!    from the expected type (`Nat`) or must match it exactly
//!    (`Word`); constructors and struct fields are looked up in the
//!    layout (whose member types are already substituted) and encoded
//!    recursively; ascriptions must equal the expected type.  The bit
//!    pattern assembly itself is [`TypeLayoutIndex::encode_ctor`] and
//!    the field ordering below — the same rules `sim` uses.

use bstr::ByteSlice;

use virdant::common::Width;
use virdant::sim::BitPat;
use virdant::typelayout::TypeLayoutIndex;
use virdant::types::typ::Type;

use crate::ast::ValueExpr;
use crate::resolve::Resolver;

/// Infer the type of `expr`, or `None` when the expression does not
/// determine its own type (a bare `Nat` literal, an anonymous struct
/// literal, an uninferable constructor application).  Returns the
/// fully-resolved type when inference succeeds.
pub fn infer_type(resolver: &Resolver, expr: &ValueExpr) -> Result<Option<Type>, String> {
    match expr {
        ValueExpr::WordLit(_, w) => Ok(Some(Type::Word(*w))),
        ValueExpr::BitLit(_) => Ok(Some(Type::Bit)),
        ValueExpr::NatLit(_) => Ok(None),
        ValueExpr::Ascribe { typ, .. } => resolver.resolve_type(typ).map(Some),
        ValueExpr::StructLit { name: Some(name), .. } => {
            let sym = resolver.find_struct(name)?;
            let typedef = resolver_typedef(resolver, sym);
            if typedef.params.is_empty() {
                Ok(Some(Type::Usual(sym, vec![])))
            } else {
                // `${Name(...)}` carries no type arguments; a generic
                // struct's arguments must come from context.
                Ok(None)
            }
        }
        ValueExpr::StructLit { name: None, .. } => Ok(None),
        ValueExpr::Ctor { name, args } => infer_ctor(resolver, name, args),
    }
}

fn resolver_typedef(resolver: &Resolver, sym: virdant::analysis::symbols::SymbolId) -> virdant::types::typedef::TypeDef {
    resolver.db().get_typedef(sym).as_ref().clone()
}

fn infer_ctor(resolver: &Resolver, name: &str, args: &[ValueExpr]) -> Result<Option<Type>, String> {
    let ctor = resolver.find_ctor(name, args.len())?;
    let mut def_args: Vec<Option<Type>> = vec![None; ctor.typedef.params.len()];

    for (param_type, arg) in ctor.param_types.iter().zip(args.iter()) {
        let inferred = infer_type(resolver, arg)?;
        let Some(inferred) = inferred else { continue };
        if !unify(param_type, &inferred, &mut def_args) {
            return Err(format!(
                "constructor '@{name}': payload of type {} does not match its declared parameter type {}",
                render_type(&inferred),
                render_type(param_type),
            ));
        }
    }

    // Substitute what we know; any remaining undetermined parameter
    // means the type is not fully determined by the payload.
    let mut out = Vec::with_capacity(def_args.len());
    for arg in def_args.into_iter() {
        match arg {
            Some(t) => out.push(t),
            None => return Ok(None),
        }
    }
    Ok(Some(Type::Usual(ctor.typedef_sym, out)))
}

/// First-order unification of a written parameter type against an
/// inferred payload type, assigning the typedef's generic parameters
/// (`def_args`) in place.  Returns false on a conflict.
fn unify(param: &Type, inferred: &Type, def_args: &mut Vec<Option<Type>>) -> bool {
    match (param, inferred) {
        (Type::Param(id), _) => {
            let slot = id.0 as usize;
            match &def_args[slot] {
                Some(existing) => existing == inferred,
                None => {
                    def_args[slot] = Some(inferred.clone());
                    true
                }
            }
        }
        (Type::Usual(s1, ps), Type::Usual(s2, ins)) => {
            s1 == s2
                && ps.len() == ins.len()
                && ps.iter().zip(ins.iter()).all(|(p, i)| unify(p, i, def_args))
        }
        (p, i) => p == i,
    }
}

/// Render a `Type` for error messages (fully-qualified via its symbol
/// when we can; falls back to the debug form).
fn render_type(typ: &Type) -> String {
    match typ {
        Type::Bit => "Bit".to_string(),
        Type::Clock => "Clock".to_string(),
        Type::Reset => "Reset".to_string(),
        Type::Word(w) => format!("Word[{w}]"),
        Type::Param(id) => format!("Param[{}]", id.0),
        Type::Usual(..) => format!("{typ:?}"),
    }
}

/// Encode `expr` as a bit pattern of `expected`, walking the layout.
pub fn encode(
    resolver: &Resolver,
    layouts: &TypeLayoutIndex,
    expected: &Type,
    expr: &ValueExpr,
) -> Result<BitPat, String> {
    match expr {
        ValueExpr::Ascribe { expr, typ } => {
            let ascribed = resolver.resolve_type(typ)?;
            if ascribed != *expected {
                return Err(format!(
                    "ascribed type {} does not match expected type {}",
                    type_display(resolver, layouts, &ascribed),
                    type_display(resolver, layouts, expected),
                ));
            }
            encode(resolver, layouts, expected, expr)
        }
        ValueExpr::NatLit(v) => match expected {
            Type::Word(w) => nat_pattern(*v, *w, &render_type(expected)),
            Type::Clock | Type::Reset => nat_pattern(*v, 1, &render_type(expected)),
            Type::Bit => Err("use `true`/`false` for a Bit value, not an integer".to_string()),
            Type::Usual(..) => Err(format!(
                "expected type {}, got an integer literal",
                type_display(resolver, layouts, expected)
            )),
            Type::Param(_) => Err("cannot encode a generic parameter".to_string()),
        },
        ValueExpr::WordLit(v, w) => match expected {
            Type::Word(w2) if w == w2 => nat_pattern(*v, *w, &render_type(expected)),
            Type::Word(w2) => Err(format!(
                "word literal is {w} bits wide, but expected type is {w2} bits wide"
            )),
            Type::Clock | Type::Reset => {
                if *w == 1 {
                    nat_pattern(*v, 1, &render_type(expected))
                } else {
                    Err(format!(
                        "word literal is {w} bits wide, but expected type is 1 bit wide"
                    ))
                }
            }
            _ => Err(format!(
                "expected type {}, got a word literal",
                type_display(resolver, layouts, expected)
            )),
        },
        ValueExpr::BitLit(b) => match expected {
            Type::Bit => Ok(BitPat::bit(*b)),
            _ => Err(format!(
                "expected type {}, got a Bit literal (`{}`)",
                type_display(resolver, layouts, expected),
                if *b { "true" } else { "false" },
            )),
        },
        ValueExpr::Ctor { name, args } => encode_ctor(resolver, layouts, expected, name, args),
        ValueExpr::StructLit { name, fields } => {
            encode_struct(resolver, layouts, expected, name.as_deref(), fields)
        }
    }
}

fn nat_pattern(v: u128, width: Width, what: &str) -> Result<BitPat, String> {
    // Width 128 always fits (v is a u128); narrower widths check.
    if width < 128 && v >= (1u128 << width) {
        return Err(format!("integer literal {v} does not fit in {width} bits ({what})"));
    }
    Ok(bitpat_u128(width, v))
}

fn encode_ctor(
    resolver: &Resolver,
    layouts: &TypeLayoutIndex,
    expected: &Type,
    name: &str,
    args: &[ValueExpr],
) -> Result<BitPat, String> {
    let Type::Usual(..) = expected else {
        return Err(format!(
            "expected type {}, got a constructor '@{name}'",
            type_display(resolver, layouts, expected)
        ));
    };
    let typ_id = layouts.type_id(expected);

    // Is this type a struct?  Structs have no constructors.
    if layouts.fields(expected).is_some() {
        return Err(format!(
            "type {} is a struct, not a union or enum; use a struct literal",
            type_display(resolver, layouts, expected)
        ));
    }

    let Some((ctor_sym, param_types)) = layouts.ctor_slot_by_name(typ_id, name) else {
        return Err(format!(
            "type {} has no constructor '@{name}'",
            type_display(resolver, layouts, expected)
        ));
    };
    if param_types.len() != args.len() {
        return Err(format!(
            "constructor '@{name}' of type {} expects {} parameter{}, got {}",
            type_display(resolver, layouts, expected),
            param_types.len(),
            if param_types.len() == 1 { "" } else { "s" },
            args.len(),
        ));
    }
    let mut arg_pats = Vec::with_capacity(args.len());
    for (param_type, arg) in param_types.iter().zip(args.iter()) {
        arg_pats.push(encode(resolver, layouts, param_type, arg)?);
    }
    layouts.encode_ctor(typ_id, ctor_sym, &arg_pats).ok_or_else(|| {
        format!(
            "internal error: failed to encode constructor '@{name}' of {}",
            type_display(resolver, layouts, expected)
        )
    })
}

fn encode_struct(
    resolver: &Resolver,
    layouts: &TypeLayoutIndex,
    expected: &Type,
    name: Option<&str>,
    fields: &[(String, ValueExpr)],
) -> Result<BitPat, String> {
    let Type::Usual(..) = expected else {
        return Err(format!(
            "expected type {}, got a struct literal",
            type_display(resolver, layouts, expected)
        ));
    };
    let Some(declared) = layouts.fields(expected) else {
        return Err(format!(
            "type {} is not a struct",
            type_display(resolver, layouts, expected)
        ));
    };

    // A named struct literal (`${Name(...)}`) must name this type.
    if let Some(name) = name {
        let sym = resolver.find_struct(name)?;
        let Type::Usual(expected_sym, _) = expected else {
            return Err(format!("internal error: expected type is not Usual"));
        };
        if sym != *expected_sym {
            return Err(format!(
                "struct literal '{name}' does not name the expected type {}",
                type_display(resolver, layouts, expected)
            ));
        }
    }

    // Every declared field must be assigned exactly once; no extras.
    let mut assigned: Vec<Option<&ValueExpr>> = vec![None; declared.len()];
    for (field_name, value) in fields {
        match declared.iter().position(|f| f.name.as_bstr() == field_name.as_bytes()) {
            Some(i) => {
                if assigned[i].is_some() {
                    return Err(format!(
                        "struct field '{field_name}' assigned more than once"
                    ));
                }
                assigned[i] = Some(value);
            }
            None => {
                return Err(format!(
                    "type {} has no field '{field_name}'",
                    type_display(resolver, layouts, expected)
                ));
            }
        }
    }
    for (i, slot) in declared.iter().enumerate() {
        if assigned[i].is_none() {
            return Err(format!(
                "struct field '{}' of type {} is not assigned",
                slot.name.to_str_lossy(),
                type_display(resolver, layouts, expected)
            ));
        }
    }

    // First field in the LOWEST bits (declaration order reversed),
    // filling the width exactly — the struct half of the shared layout
    // rules, matching `sim`'s `eval_struct` and the Verilog backend's
    // `ExprField` bit offsets.
    let mut pats = Vec::with_capacity(declared.len());
    for (i, slot) in declared.iter().enumerate() {
        let value = assigned[i].unwrap();
        pats.push(encode(resolver, layouts, &slot.param_types[0], value)?);
    }
    let mut out = BitPat::zeros(0);
    for pat in pats.into_iter().rev() {
        out = out.concat_msb_first(&pat);
    }
    Ok(out)
}

/// Build a `BitPat` of `width` bits holding `value` (any width, not
/// just the 64-bit fast path).
pub fn bitpat_u128(width: Width, value: u128) -> BitPat {
    if width <= 64 {
        BitPat::from_u64(width, value as u64)
    } else {
        let mut out = BitPat::zeros(width);
        for i in 0..width as usize {
            out.set_bit(i, (value >> i) & 1 != 0);
        }
        out
    }
}

/// The user-facing display of a type: fully-qualified for user types
/// (`pkg::Name[args]`), bare for the builtin package.
pub fn type_display(
    resolver: &Resolver,
    layouts: &TypeLayoutIndex,
    typ: &Type,
) -> String {
    match typ {
        Type::Bit => "Bit".to_string(),
        Type::Clock => "Clock".to_string(),
        Type::Reset => "Reset".to_string(),
        Type::Word(w) => format!("Word[{w}]"),
        Type::Param(_) => "<generic>".to_string(),
        Type::Usual(sym, args) => {
            let fqn = resolver
                .db()
                .get_symboltable()
                .symbol(*sym)
                .fqn()
                .to_str_lossy()
                .into_owned();
            let name = match fqn.strip_prefix("builtin::") {
                Some(simple) => simple.to_string(),
                None => fqn,
            };
            if args.is_empty() {
                name
            } else {
                let args: Vec<String> = args
                    .iter()
                    .map(|a| type_display(resolver, layouts, a))
                    .collect();
                format!("{name}[{}]", args.join(", "))
            }
        }
    }
}
