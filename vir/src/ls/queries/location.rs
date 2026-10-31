//! Canonical location JSON shared by the location-bearing queries.

use serde_json::{json, Value};

use virdant::analysis::Location;
use virdant::common::source::Span;
use virdant::package::PackageId;

use super::project::Project;

/// The canonical JSON shape for a source position: the file path plus
/// 1-based start/end line and column (matching how `vir` reports
/// diagnostics).
pub fn span_json(project: &Project, package: PackageId, span: Span) -> Value {
    json!({
        "file": project.file_json_path(package),
        "line": span.start().line(),
        "col": span.start().col(),
        "end_line": span.end().line(),
        "end_col": span.end().col(),
    })
}

/// [`span_json`] for a `Location` (package + AST node): resolves the
/// node's span from its parsing.
pub fn location_json(project: &Project, location: Location) -> Value {
    let parsing = project.db.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    span_json(project, location.package(), node.span())
}
