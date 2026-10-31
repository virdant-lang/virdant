//! The type-of query.

use virdant::common::source::LineCol;
use virdant::db::Db;
use virdant::package::PackageId;

use super::types::type_to_source;

/// The type information behind a `type-of` query at a location.
pub struct TypeOfInfo {
    /// The inferred type rendered as Virdant source syntax, when known.
    pub typ: Option<String>,
    /// When the type could not be inferred: either the node is not an
    /// expression, or the type checker reported diagnostics.
    pub error: Option<String>,
}

/// The type of the expression at (package, 1-based line:col).
pub fn typeof_at(db: &Db, package: PackageId, linecol: LineCol) -> Option<TypeOfInfo> {
    let parsing = db.get_parsing(package);
    let node_id = parsing.at(linecol)?;
    let node = parsing.ast_node(node_id);
    if !node.is_expr() {
        return Some(TypeOfInfo {
            typ: None,
            error: Some("not an expression".to_string()),
        });
    }
    match db.get_typeof(node.location()) {
        Ok(typ) => Some(TypeOfInfo {
            typ: Some(type_to_source(db, package, &typ)),
            error: None,
        }),
        Err(diags) => Some(TypeOfInfo {
            typ: None,
            error: Some(
                diags.iter().map(|diag| diag.message().to_string()).collect::<Vec<_>>().join("; "),
            ),
        }),
    }
}
