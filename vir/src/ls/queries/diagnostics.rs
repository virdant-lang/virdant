//! The diagnostics query.

use virdant::common::source::Span;
use virdant::db::Db;
use virdant::diagnostics::DiagnosticLevel;
use virdant::package::PackageId;

/// One project diagnostic, as served by the `diagnostics` query.
pub struct DiagnosticEntry {
    pub package: PackageId,
    pub span: Span,
    pub level: &'static str,
    pub message: String,
}

/// All project diagnostics, like `vir check` sees.  Note: the query
/// itself always succeeds; errors are data, not tool failure.
pub fn diagnostics(db: &Db) -> Vec<DiagnosticEntry> {
    db.check().iter().map(|diag| DiagnosticEntry {
        package: diag.region().package(),
        span: diag.region().span(),
        level: match diag.level() {
            DiagnosticLevel::Error => "error",
            DiagnosticLevel::Warning => "warning",
            DiagnosticLevel::Info => "info",
        },
        message: diag.message().to_string(),
    }).collect()
}
