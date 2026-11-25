//! The set of annotation names Virdant knows about.
//!
//! Every `@name` annotation appearing in a package must use a name from
//! `KNOWN_ANNOTATIONS`; unknown names are an error (see
//! `queries::check::check_annotations`).  This table is the single source
//! of truth shared by the checker and any consumer enumerating
//! annotations.

use bstr::{BStr, BString};
use bstr::ByteSlice as _;

use crate::common::source::Region;
use crate::syntax::ast::AstNode;
use crate::syntax::parsing::Parsing;

/// Names of all annotations the compiler consumes.  The check is
/// name-level only: a known name in the wrong place (e.g. `@pin` on a
/// moddef) is a separate, future misplaced-annotation concern.
pub const KNOWN_ANNOTATIONS: &[&str] = &[
    // Platform definition:
    "noinitial",
    "fpga",
    "part",
    // Platform members:
    "pin",
    "period_ns",
    // External (`ext`) moddefs:
    "cell",
    // Moddefs and builtins:
    "parameter",
    // Moddefs: disables the clock checker for the module; every
    // component must then carry an explicit `on`/`async` clause.
    "unsafe_clocks",
    // Memory declarations:
    "image",
    "image_hex",
    "image_inline",
    // Memory declarations: steers Yosys's block-RAM-vs-flip-flop
    // mapping decision (`ram_style` in the emitted Verilog).
    "memory_style",
];

/// The mapping intent declared by `@memory_style` on a memory
/// declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryStyle {
    /// Force the hard block-RAM mapping.  A memory whose reads are
    /// combinational cannot be mapped to block RAM; the check phase
    /// rejects that combination (`queries::check::check_memory_styles`)
    /// so the user sees a source-level diagnostic instead of Yosys's
    /// late `no valid mapping found` error.
    Block,
    /// Force the flip-flop (distributed-logic) mapping.
    Ff,
}

impl MemoryStyle {
    /// Parse a `@memory_style` string argument.  `"distributed"` is
    /// deliberately not accepted: on iCE40 it is a hard Yosys error
    /// (no LUTRAM, no fallback), so Virdant never emits it.
    pub fn from_annotation_value(value: &BStr) -> Option<MemoryStyle> {
        match value.as_bytes() {
            b"block" => Some(MemoryStyle::Block),
            b"ff" => Some(MemoryStyle::Ff),
            _ => None,
        }
    }

    /// The value of the Yosys `ram_style` attribute this style lowers
    /// to.  Virdant spells the flip-flop mapping `"ff"`; Yosys's
    /// spelling (the one `memory_libmap` honors, on RAMs and ROMs
    /// alike) is `"logic"`.
    pub fn yosys_value(self) -> &'static str {
        match self {
            MemoryStyle::Block => "block",
            MemoryStyle::Ff => "logic",
        }
    }
}

/// The raw `@memory_style("...")` argument of a memory declaration: the
/// string literal's text between the quotes (not interpreted — call
/// `MemoryStyle::from_annotation_value` for that), and the argument's
/// region (for diagnostics).  `None` when the declaration has no
/// `@memory_style` annotation (or its argument is not a string
/// literal).
pub fn memory_style_annotation(
    parsing: &Parsing,
    decl: &AstNode<'_>,
) -> Option<(BString, Region)> {
    for annotation in decl.annotations() {
        let Some(name) = annotation.annotation_name() else {
            continue;
        };
        if parsing.string(name).as_bytes() != b"memory_style" {
            continue;
        }
        let Some((value, region)) = annotation.annotation_str_arg() else {
            return None;
        };
        return Some((value, region));
    }
    None
}

pub fn is_known_annotation(name: &str) -> bool {
    KNOWN_ANNOTATIONS.contains(&name)
}
