//! Round-trippable value rendering for `vir poke`.
//!
//! Every value prints in the literal syntax the script language
//! accepts, so a printed value can be pasted into a `poke` or an
//! `assert` unchanged:
//!
//! - `Word[N]` renders as a full-width binary literal, `0b00000101`
//!   (leading zeros included, so the width is recoverable);
//! - `Bit` renders as `true` / `false`;
//! - `Clock` is opaque: it has no printed value (time is driven by
//!   `tick`, not by values); `Reset` renders as `0b0` / `0b1`;
//! - enums and unions render as their constructor syntax, recursively:
//!   `@Valid(0b00000000)`, `@Invalid`;
//! - structs render as struct literals, recursively: `${f = value, ...}`;
//! - a bit pattern that is not a valid inhabitant of its type prints
//!   as `<invalid>`.

use virdant::sim::{Sim, Value};
use virdant::types::Type;

use super::ast::Fmt;

pub fn render(sim: &Sim, value: &Value) -> String {
    render_fmt(sim, value, None)
}

/// [`render`], with a per-signal display radix applied (see
/// `Session::formats` for the `Option<Fmt>` semantics).  The radix
/// only affects a top-level `Word` value (what a Word signal's
/// `show`/`peek` row carries, or a memory's words); every other type
/// — and words nested inside structs/unions — keeps the
/// round-trippable rendering.
pub fn render_fmt(sim: &Sim, value: &Value, fmt: Option<Fmt>) -> String {
    let layouts = sim.layouts();
    if let (Type::Word(_), Some(fmt)) = (layouts.type_of(value.typ_id()), fmt) {
        return render_word_fmt(value, fmt);
    }
    render_default(sim, value)
}

/// One Word value in a chosen radix.
fn render_word_fmt(value: &Value, fmt: Fmt) -> String {
    match fmt {
        Fmt::Bin => format!("0b{}", value.to_bit_string()),
        Fmt::Hex => {
            // Whole nibbles, zero-padded, so the width stays
            // recoverable from the rendering.
            let bits = value.to_bit_string();
            let padded = "0".repeat((4 - bits.len() % 4) % 4) + &bits;
            let hex: String = padded
                .as_bytes()
                .chunks(4)
                .map(|chunk| {
                    let mut nibble = 0u32;
                    for c in chunk {
                        nibble = (nibble << 1) | u32::from(*c == b'1');
                    }
                    char::from_digit(nibble, 16).unwrap()
                })
                .collect();
            format!("0x{hex}")
        }
        Fmt::Dec => format!("{}", decimal(value)),
    }
}

/// The value as decimal digits, any width (binary long multiplication
/// into a digit vector — no 64-bit ceiling).
fn decimal(value: &Value) -> String {
    let mut digits: Vec<u8> = vec![0];
    for i in (0..value.width()).rev() {
        // digits = digits * 2 + bit.
        let mut carry = if value.get_bit(i as usize) { 1 } else { 0 };
        for d in digits.iter_mut() {
            let doubled = 2 * *d + carry;
            *d = doubled % 10;
            carry = doubled / 10;
        }
        while carry > 0 {
            digits.push(carry % 10);
            carry /= 10;
        }
    }
    digits
        .iter()
        .rev()
        .map(|d| (b'0' + d) as char)
        .collect()
}

fn render_default(sim: &Sim, value: &Value) -> String {
    let layouts = sim.layouts();
    match layouts.type_of(value.typ_id()) {
        Type::Bit => {
            if value.get_bit(0) { "true".to_string() } else { "false".to_string() }
        }
        Type::Clock => "<opaque>".to_string(),
        Type::Reset => {
            format!("0b{}", value.to_bit_string())
        }
        Type::Word(_) => {
            format!("0b{}", value.to_bit_string())
        }
        Type::Param(_) => "<invalid>".to_string(),
        Type::Usual(..) => {
            // Enums and unions decode through their constructor; a
            // pattern that does not decode is invalid (unknown tag or
            // non-zero padding).
            if let Some(ctor) = sim.decode_ctor(value) {
                if ctor.payloads.is_empty() {
                    format!("@{}", ctor.name)
                } else {
                    let args: Vec<String> = ctor
                        .payloads
                        .iter()
                        .map(|payload| render(sim, payload))
                        .collect();
                    format!("@{}({})", ctor.name, args.join(", "))
                }
            } else if let Some(fields) = sim.decode_struct_fields(value) {
                let rendered: Vec<String> = fields
                    .iter()
                    .map(|(name, field)| format!("{} = {}", name, render(sim, field)))
                    .collect();
                format!("${{{}}}", rendered.join(", "))
            } else {
                "<invalid>".to_string()
            }
        }
    }
}
