//! The hover query.

use bstr::ByteSlice as _;

use virdant::common::source::{LineCol, Span};
use virdant::db::Db;
use virdant::package::PackageId;

use super::types::type_to_source;

/// The data behind a hover: the AST node at the location plus, when the
/// node is an expression, its inferred type in both the raw `{:?}`
/// rendering (what the LSP hover prints) and Virdant source syntax
/// (what the JSON contract carries).
pub struct HoverInfo {
    /// The span of the hovered node, for the CLI's human `Location :`
    /// line (the JSON contract deliberately carries no location).
    pub span: Span,
    pub spelling: String,
    pub summary: String,
    pub typof_debug: Option<String>,
    pub typof_source: Option<String>,
}

/// Hover at a (package, 1-based line:col): the AST node there, with the
/// inferred type when it is an expression.
pub fn hover_at(db: &Db, package: PackageId, linecol: LineCol) -> Option<HoverInfo> {
    let parsing = db.get_parsing(package);
    let node_id = parsing.at(linecol)?;
    let node = parsing.ast_node(node_id);
    let typof = if node.is_expr() {
        db.get_typeof(node.location()).ok()
    } else {
        None
    };
    Some(HoverInfo {
        span: node.span(),
        spelling: node.spelling().to_str_lossy().into_owned(),
        summary: node.summary(),
        typof_debug: typof.as_ref().map(|typof| format!("{typof:?}")),
        typof_source: typof.map(|typof| type_to_source(db, package, &typof)),
    })
}
