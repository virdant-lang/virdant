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
//! - structs render as their source-style literal syntax, recursively:
//!   `${f = value, ...}`;
//! - a bit pattern that is not a valid inhabitant of its type prints
//!   as `<invalid>`.

use virdant::sim::{Sim, Value};
use virdant::types::Type;

pub fn render(sim: &Sim, value: &Value) -> String {
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
