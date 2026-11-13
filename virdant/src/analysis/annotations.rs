//! The set of annotation names Virdant knows about.
//!
//! Every `@name` annotation appearing in a package must use a name from
//! `KNOWN_ANNOTATIONS`; unknown names are an error (see
//! `queries::check::check_annotations`).  This table is the single source
//! of truth shared by the checker and any consumer enumerating
//! annotations.

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
    // Memory declarations:
    "image",
    "image_hex",
    "image_inline",
];

pub fn is_known_annotation(name: &str) -> bool {
    KNOWN_ANNOTATIONS.contains(&name)
}
