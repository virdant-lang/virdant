//! The `vir poke` expression evaluator.
//!
//! A small evaluator over [`Value`]s straight out of the simulation —
//! not a reuse of the compile-time expression machinery — but the type
//! information it needs (bit widths, struct/enum layouts) comes from
//! the elaborated circuit through `Sim`.
//!
//! Evaluation is bidirectional: a bare `Nat` literal (`42`) has no
//! width of its own and takes it from context (the other side of a
//! comparison, or a `poke` target), and an enum constructor (`@Foo`)
//! takes its type from context the same way.  Where no context exists,
//! a `Nat` literal materializes as a `Word` of its minimal width.

use bstr::ByteSlice;
use virdant::sim::{BitPat, Value};
use virdant::types::Type;

use super::ast::{BinKind, Expr, PathStart};
use super::Session;

/// An in-flight evaluation result: a fully-typed simulation value, an
/// unwidthed literal, a widthed word literal, or an aggregate literal
/// (constructor or struct) still waiting for its expected type.
#[derive(Clone)]
enum EVal {
    V(Value),
    Nat(u128),
    Word(u128, u16),
    Ctor(String, Vec<EVal>),
    StructLit {
        fields: Vec<(String, EVal)>,
    },
}

/// Evaluate `expr` with no expected type (e.g. for `print`).
pub fn eval(session: &Session, expr: &Expr) -> Result<Value, String> {
    let ev = eval_ev(session, expr, None)?;
    materialize(session, ev, None)
}

/// Evaluate `expr`, coercing the result to `expected` (e.g. the type
/// of a `poke` target).
pub fn eval_typed(session: &Session, expr: &Expr, expected: &Type) -> Result<Value, String> {
    let ev = eval_ev(session, expr, Some(expected))?;
    materialize(session, ev, Some(expected))
}

fn eval_ev(session: &Session, expr: &Expr, expected: Option<&Type>) -> Result<EVal, String> {
    match expr {
        Expr::Path(spath) => {
            if spath.start == PathStart::It && spath.segments.is_empty() {
                session.it.clone().map(EVal::V).ok_or_else(|| {
                    "`it` has no value yet: peek a signal first".to_string()
                })
            } else {
                Ok(EVal::V(session.get_value(spath)?))
            }
        }
        Expr::NatLit(v) => match expected {
            Some(typ) => Ok(EVal::V(materialize(session, EVal::Nat(*v), Some(typ))?)),
            None => Ok(EVal::Nat(*v)),
        },
        Expr::WordLit(v, w) => match expected {
            Some(typ) => Ok(EVal::V(materialize(session, EVal::Word(*v, *w), Some(typ))?)),
            None => Ok(EVal::Word(*v, *w)),
        },
        Expr::BitLit(b) => {
            let typ_id = session.sim.layouts().type_id(&Type::Bit);
            Ok(EVal::V(Value::from_bool(typ_id, *b)))
        }
        Expr::Ctor { name, args } => {
            let args: Vec<EVal> = args
                .iter()
                .map(|arg| eval_ev(session, arg, None))
                .collect::<Result<_, _>>()?;
            let ev = EVal::Ctor(name.clone(), args);
            match expected {
                Some(typ) => Ok(EVal::V(materialize(session, ev, Some(typ))?)),
                None => Ok(ev),
            }
        }
        Expr::StructLit { fields } => {
            let fields: Vec<(String, EVal)> = fields
                .iter()
                .map(|(field, expr)| {
                    Ok::<(String, EVal), String>((field.clone(), eval_ev(session, expr, None)?))
                })
                .collect::<Result<_, _>>()?;
            let ev = EVal::StructLit { fields };
            match expected {
                Some(typ) => Ok(EVal::V(materialize(session, ev, Some(typ))?)),
                None => Ok(ev),
            }
        }
        Expr::Neg(subject) => eval_arith_unop(session, subject, expected, BitPat::neg),
        Expr::Inv(subject) => eval_arith_unop(session, subject, expected, BitPat::not),
        Expr::Not(subject) => {
            let subject = eval_ev(session, subject, None)?;
            let truthy = truthy(&subject)?;
            let value = Value::from_bool(session.sim.layouts().type_id(&Type::Bit), !truthy);
            materialize(session, EVal::V(value), expected).map(EVal::V)
        }
        Expr::Index(subject, i) => {
            let subject = eval_ev(session, subject, None)?;
            let subject = materialize(session, subject, None)?;
            let width = subject.width();
            if *i >= width {
                return Err(format!(
                    "index {} out of range: the value has {} bit(s)",
                    i, width
                ));
            }
            let typ_id = session.sim.layouts().type_id(&Type::Bit);
            Ok(EVal::V(Value::from_bool(typ_id, subject.get_bit(*i as usize))))
        }
        Expr::Slice(subject, hi, lo) => {
            let subject = eval_ev(session, subject, None)?;
            let subject = materialize(session, subject, None)?;
            let width = subject.width();
            if *lo > *hi {
                return Err(format!(
                    "invalid slice [{}..{}]: the top index must not be below the bottom index",
                    hi, lo
                ));
            }
            if *hi > width {
                return Err(format!(
                    "slice [{}..{}] out of range: the value has {} bit(s)",
                    hi, lo, width
                ));
            }
            let bits = subject.bitpat().slice(*lo, *hi);
            let typ = Type::Word(hi - lo);
            Ok(EVal::V(Value::new(session.sim.layouts().type_id(&typ), bits)))
        }
        Expr::Bin(op, lhs, rhs) => match op {
            BinKind::LogAnd | BinKind::LogOr | BinKind::LogXor => {
                let lhs = eval_ev(session, lhs, None)?;
                let rhs = eval_ev(session, rhs, None)?;
                let a = truthy(&lhs)?;
                let b = truthy(&rhs)?;
                let result = match op {
                    BinKind::LogAnd => a && b,
                    BinKind::LogOr => a || b,
                    _ => a ^ b,
                };
                let value = Value::from_bool(session.sim.layouts().type_id(&Type::Bit), result);
                materialize(session, EVal::V(value), expected).map(EVal::V)
            }
            BinKind::Eq | BinKind::Neq | BinKind::Lt | BinKind::Lte | BinKind::Gt
            | BinKind::Gte => {
                let lhs = eval_ev(session, lhs, None)?;
                let rhs = eval_ev(session, rhs, None)?;
                let (lv, rv) = coerce_comparison(session, lhs, rhs)?;
                let ord = lv.bitpat().ucmp(&rv.bitpat());
                let result = match op {
                    BinKind::Eq => lv.bitpat() == rv.bitpat(),
                    BinKind::Neq => lv.bitpat() != rv.bitpat(),
                    BinKind::Lt => ord == std::cmp::Ordering::Less,
                    BinKind::Lte => ord != std::cmp::Ordering::Greater,
                    BinKind::Gt => ord == std::cmp::Ordering::Greater,
                    BinKind::Gte => ord != std::cmp::Ordering::Less,
                    _ => unreachable!(),
                };
                let value = Value::from_bool(session.sim.layouts().type_id(&Type::Bit), result);
                materialize(session, EVal::V(value), expected).map(EVal::V)
            }
            BinKind::Add | BinKind::Sub | BinKind::And | BinKind::Or | BinKind::Xor => {
                let lhs = eval_ev(session, lhs, None)?;
                let rhs = eval_ev(session, rhs, None)?;
                let (lv, rv) = coerce_pair(session, lhs, rhs)?;
                let width = lv.width();
                let lhs_pat = lv.bitpat();
                let rhs_pat = rv.bitpat();
                let result = match op {
                    BinKind::Add => lhs_pat.add(&rhs_pat),
                    BinKind::Sub => lhs_pat.sub(&rhs_pat),
                    BinKind::And => lhs_pat.and(&rhs_pat),
                    BinKind::Or => lhs_pat.or(&rhs_pat),
                    BinKind::Xor => lhs_pat.xor(&rhs_pat),
                    _ => unreachable!(),
                };
                let typ = Type::Word(width);
                let value = Value::new(session.sim.layouts().type_id(&typ), result);
                materialize(session, EVal::V(value), expected).map(EVal::V)
            }
        },
    }
}

/// Apply `-` or `~` to a value (or a literal, at its intrinsic width).
fn eval_arith_unop(
    session: &Session,
    subject: &Expr,
    expected: Option<&Type>,
    op: fn(&BitPat) -> BitPat,
) -> Result<EVal, String> {
    let subject = eval_ev(session, subject, None)?;
    let subject = materialize(session, subject, None)?;
    let width = subject.width();
    let result = op(&subject.bitpat());
    let typ = Type::Word(width);
    let value = Value::new(session.sim.layouts().type_id(&typ), result);
    materialize(session, EVal::V(value), expected).map(EVal::V)
}

/// Truthiness, matching the simulator: a 1-bit value is its bit 0,
/// anything wider is non-zero.
fn truthy(ev: &EVal) -> Result<bool, String> {
    match ev {
        EVal::V(value) => Ok(if value.width() == 1 {
            value.get_bit(0)
        } else {
            !value.is_zero()
        }),
        EVal::Nat(v) | EVal::Word(v, _) => Ok(*v != 0),
        EVal::Ctor(name, _) => Err(format!(
            "constructor @{} cannot be used as a condition",
            name
        )),
        EVal::StructLit { .. } => Err(
            "struct literal cannot be used as a condition".to_string()
        ),
    }
}

/// Coerce the two sides of a comparison to a common representation.
///
/// An aggregate literal (constructor or struct) adopts the other
/// side's full type (so the payloads of `@Valid(x)` coerce to the
/// union's parameter types, and the fields of a struct literal to the
/// struct's field types); two plain literals land on a common word
/// width; anything else must already agree (checked by `materialize`).
fn coerce_comparison(
    session: &Session,
    lhs: EVal,
    rhs: EVal,
) -> Result<(Value, Value), String> {
    let expected: Option<Type> = match (&lhs, &rhs) {
        (EVal::V(v), EVal::Ctor(..))
        | (EVal::Ctor(..), EVal::V(v))
        | (EVal::V(v), EVal::StructLit { .. })
        | (EVal::StructLit { .. }, EVal::V(v)) => {
            Some(session.sim.layouts().type_of(v.typ_id()).clone())
        }
        _ => None,
    };
    match expected {
        Some(typ) => Ok((
            materialize(session, lhs, Some(&typ))?,
            materialize(session, rhs, Some(&typ))?,
        )),
        None => coerce_pair(session, lhs, rhs),
    }
}

/// Coerce two operands to a common word width for an arithmetic or
/// bitwise operation: the typed side's width wins; two literals land
/// on the wider of their intrinsic widths.
fn coerce_pair(session: &Session, lhs: EVal, rhs: EVal) -> Result<(Value, Value), String> {
    let width = match (&lhs, &rhs) {
        (EVal::V(a), _) => a.width(),
        (_, EVal::V(b)) => b.width(),
        _ => {
            let lw = intrinsic_width(&lhs);
            let rw = intrinsic_width(&rhs);
            lw.max(rw).unwrap_or(1)
        }
    };
    let typ = Type::Word(width);
    Ok((
        materialize(session, lhs, Some(&typ))?,
        materialize(session, rhs, Some(&typ))?,
    ))
}

/// The width a literal already carries (`Nat` literals: the minimal
/// width that fits; constructors: none).
fn intrinsic_width(ev: &EVal) -> Option<u16> {
    match ev {
        EVal::V(value) => Some(value.width()),
        EVal::Nat(v) => Some(min_width(*v)),
        EVal::Word(_, w) => Some(*w),
        EVal::Ctor(..) | EVal::StructLit { .. } => None,
    }
}

fn min_width(v: u128) -> u16 {
    if v == 0 {
        1
    } else {
        (128 - v.leading_zeros()) as u16
    }
}

/// Turn `ev` into a concrete [`Value`], coercing to `expected` when
/// given.
fn materialize(session: &Session, ev: EVal, expected: Option<&Type>) -> Result<Value, String> {
    let layouts = session.sim.layouts();
    match expected {
        None => match ev {
            EVal::V(value) => Ok(value),
            EVal::Nat(v) => Ok(word_value(session, min_width(v), v)),
            EVal::Word(v, w) => Ok(word_value(session, w, v)),
            EVal::Ctor(name, _) => Err(format!(
                "constructor @{name} needs a typed context: compare it against a signal or poke it into one"
            )),
            EVal::StructLit { .. } => Err(
                "struct literal needs a typed context: compare it against a signal or poke it into one".to_string()
            ),
        },
        Some(typ) => match typ {
            Type::Word(width) => match ev {
                EVal::V(value) => {
                    if value.typ_id() == layouts.type_id(typ) {
                        Ok(value)
                    } else if is_primitive(layouts.type_of(value.typ_id()))
                        && value.width() == *width
                    {
                        Ok(retag(session, value, typ))
                    } else {
                        Err(format!(
                            "type mismatch: expected Word[{}], got {} ({} bit(s))",
                            width,
                            type_name(session, layouts.type_of(value.typ_id())),
                            value.width()
                        ))
                    }
                }
                EVal::Nat(v) => {
                    if fits(v, *width) {
                        Ok(word_value(session, *width, v))
                    } else {
                        Err(format!(
                            "literal {v} does not fit Word[{width}]"
                        ))
                    }
                }
                EVal::Word(v, _) => {
                    if fits(v, *width) {
                        Ok(word_value(session, *width, v))
                    } else {
                        Err(format!(
                            "literal {v} does not fit Word[{width}]"
                        ))
                    }
                }
                EVal::Ctor(name, _) => Err(format!(
                    "type mismatch: @{name} is a constructor, but a Word[{width}] was expected"
                )),
                EVal::StructLit { .. } => Err(format!(
                    "type mismatch: a struct literal cannot be used where a Word[{width}] was expected"
                )),
            },
            Type::Bit | Type::Clock | Type::Reset => match ev {
                EVal::V(value) => {
                    if value.typ_id() == layouts.type_id(typ) {
                        Ok(value)
                    } else if is_primitive(layouts.type_of(value.typ_id()))
                        && value.width() == 1
                    {
                        Ok(retag(session, value, typ))
                    } else {
                        Err(format!(
                            "type mismatch: expected {}, got {}",
                            type_name(session, typ),
                            type_name(session, layouts.type_of(value.typ_id()))
                        ))
                    }
                }
                EVal::Nat(v) if v <= 1 => {
                    Ok(Value::from_bool(layouts.type_id(typ), v == 1))
                }
                EVal::Word(v, _) if v <= 1 => {
                    Ok(Value::from_bool(layouts.type_id(typ), v == 1))
                }
                EVal::Nat(v) | EVal::Word(v, _) => Err(format!(
                    "literal {v} does not fit a 1-bit type"
                )),
                EVal::Ctor(name, _) => Err(format!(
                    "type mismatch: @{name} is a constructor, but {} was expected",
                    type_name(session, typ)
                )),
                EVal::StructLit { .. } => Err(format!(
                    "type mismatch: a struct literal cannot be used where {} was expected",
                    type_name(session, typ)
                )),
            },
            Type::Usual(..) => match ev {
                EVal::V(value) => {
                    if value.typ_id() == layouts.type_id(typ) {
                        Ok(value)
                    } else {
                        Err(format!(
                            "type mismatch: expected {}, got {}",
                            type_name(session, typ),
                            type_name(session, layouts.type_of(value.typ_id()))
                        ))
                    }
                }
                EVal::Ctor(name, args) => {
                    let typ_id = layouts.type_id(typ);
                    let Some((_, param_types)) = layouts.ctor_slot_by_name(typ_id, &name) else {
                        return Err(format!(
                            "unknown constructor @{name} for type {}",
                            type_name(session, typ)
                        ));
                    };
                    if param_types.len() != args.len() {
                        return Err(format!(
                            "constructor @{name} expects {} argument(s), got {}",
                            param_types.len(),
                            args.len()
                        ));
                    }
                    let mut values = Vec::with_capacity(args.len());
                    for (arg, param) in args.into_iter().zip(param_types.iter()) {
                        values.push(materialize(session, arg, Some(param))?);
                    }
                    session
                        .sim
                        .encode_ctor_by_name(typ, &name, &values)
                        .ok_or_else(|| {
                            format!("cannot encode @{name} as {}", type_name(session, typ))
                        })
                }
                EVal::StructLit { fields } => {
                    materialize_struct(session, typ, fields)
                }
                EVal::Nat(_) | EVal::Word(..) => Err(format!(
                    "type mismatch: a plain literal cannot be used where {} was expected",
                    type_name(session, typ)
                )),
            },
            Type::Param(_) => Err("cannot coerce to a generic parameter type".to_string()),
        },
    }
}

/// Materialize a struct literal against the struct type `typ`: every
/// declared field must be given exactly once (in any order); each
/// field expression is materialized against its declared type.
fn materialize_struct(
    session: &Session,
    typ: &Type,
    fields: Vec<(String, EVal)>,
) -> Result<Value, String> {
    let layouts = session.sim.layouts();
    let declared = layouts.fields(typ).ok_or_else(|| {
        format!(
            "type mismatch: {} is not a struct, but a struct literal was given",
            type_name(session, typ)
        )
    })?;
    let mut values = Vec::with_capacity(declared.len());
    for slot in declared {
        let field_name = String::from_utf8_lossy(slot.name.as_bstr()).into_owned();
        let given: Vec<&EVal> = fields
            .iter()
            .filter(|(given, _)| *given == field_name)
            .map(|(_, ev)| ev)
            .collect();
        match given[..] {
            [] => {
                return Err(format!(
                    "struct literal is missing field {field_name}"
                ))
            }
            [field] => {
                let value =
                    materialize(session, field.clone(), Some(&slot.param_types[0]))?;
                values.push((field_name, value));
            }
            _ => {
                return Err(format!(
                    "struct literal gives field {field_name} more than once"
                ))
            }
        }
    }
    session
        .sim
        .encode_struct_by_name(typ, &values)
        .ok_or_else(|| format!("cannot encode struct literal as {}", type_name(session, typ)))
}

/// Re-tag `value` with `typ` (allowed when the widths already match
/// and `typ` is a primitive; e.g. a `Bit`-typed slice index poked into
/// a `Clock`).
fn retag(session: &Session, value: Value, typ: &Type) -> Value {
    let typ_id = session.sim.layouts().type_id(typ);
    if value.typ_id() == typ_id {
        value
    } else {
        Value::new(typ_id, value.bitpat())
    }
}

fn word_value(session: &Session, width: u16, v: u128) -> Value {
    let typ = Type::Word(width);
    Value::new(session.sim.layouts().type_id(&typ), pat_from_u128(width, v))
}

fn pat_from_u128(width: u16, v: u128) -> BitPat {
    if width <= 64 {
        BitPat::from_u64(width, v as u64)
    } else {
        let mut bits = Vec::with_capacity(width as usize);
        for i in (0..width).rev() {
            bits.push((v >> i) & 1 != 0);
        }
        BitPat::from_bits_msb_first(width, &bits)
    }
}

/// Does `v` fit in `width` bits?
fn fits(v: u128, width: u16) -> bool {
    width >= 128 || (v >> width) == 0
}

fn type_name(session: &Session, typ: &Type) -> String {
    virdant::types::render::render_type(&session.db, typ)
}

fn is_primitive(typ: &Type) -> bool {
    matches!(typ, Type::Bit | Type::Clock | Type::Reset | Type::Word(_))
}
