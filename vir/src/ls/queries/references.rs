//! The references query: finding references to a resolved symbol.

use bstr::{BStr, ByteSlice as _};

use virdant::analysis::Location;
use virdant::analysis::symbols::{Symbol, SymbolKind};
use virdant::db::Db;
use virdant::syntax::ast::AstNode;
use virdant::syntax::parsing::Parsing;
use virdant::syntax::payload::AstNodePayload;

/// One reference to a target, as served by the `references` query.
pub struct ReferenceEntry {
    pub location: Location,
    /// `"of"` for `Ofness` references (type positions, `of` clauses,
    /// platform `for` clauses), `"expr"` for component references in
    /// expressions, `"target"` for bare `Path` occurrences (driver
    /// targets, `unused` marks).
    pub kind: &'static str,
}

/// Best-effort reference scan for a resolved symbol.  For items, covers
/// every `Ofness` AST node naming the item (qualified or not).  For
/// components, scans the declaring module's AST for `ExprReference`
/// nodes and bare `Path` nodes spelling the component's path.
///
/// NOTE: this is not the full reference analysis — uses of a component
/// behind field access (`.port`) or socket channels driven through
/// submodule connections are not attributed here.
pub fn references(db: &Db, target: &Symbol) -> Vec<ReferenceEntry> {
    let mut out = vec![];

    // Item references: every Ofness node whose (optionally qualified)
    // name matches the target item.
    let packages = db.get_packages();
    let target_pkg_name =
        packages.name(target.location.package()).to_str_lossy().into_owned();
    let target_name = target.name.clone();
    for pkg in packages.ids() {
        let parsing = db.get_parsing(pkg);
        let mut stack = vec![parsing.root()];
        while let Some(node) = stack.pop() {
            stack.extend(node.children());
            if let AstNodePayload::Ofness(ofness) = node.payload() {
                let name_matches = parsing.string(ofness.name) == target_name.as_bstr();
                let package_matches = match ofness.package {
                    Some(pkg) => parsing.string(pkg) == target_pkg_name.as_str(),
                    None => true,
                };
                if name_matches && package_matches {
                    out.push(ReferenceEntry { location: node.location(), kind: "of" });
                }
            }
        }
    }

    // Component references: scan the declaring module's AST for
    // ExprReference and bare Path nodes spelling the component's path.
    if matches!(target.kind(), SymbolKind::Component) {
        if let Some(parent_id) = target.parent_id() {
            let parsing = db.get_parsing(target.location.package());
            let parent = db.get_symboltable().symbol(parent_id);
            let parent_node = parsing.ast_node(parent.location().ast_node_id());
            walk_component_refs(&parent_node, &parsing, target.name.as_bstr(), &mut out);
        }
    }

    out
}

/// Walks the subtree of a module definition collecting references to a
/// component: `ExprReference` nodes (the wrapped `Path` child is
/// skipped so it is not double-counted) and bare `Path` nodes (driver
/// targets, `unused` marks).
fn walk_component_refs<'p>(
    node: &AstNode<'p>,
    parsing: &Parsing,
    target_name: &BStr,
    out: &mut Vec<ReferenceEntry>,
) {
    match node.payload() {
        AstNodePayload::ExprReference => {
            if let Some(path) = node.path() {
                if parsing.string(path) == target_name {
                    out.push(ReferenceEntry { location: node.location(), kind: "expr" });
                }
            }
            // Do not descend: the wrapped Path child would double-count.
            return;
        }
        AstNodePayload::Path(path) => {
            if parsing.string(path.path) == target_name {
                out.push(ReferenceEntry { location: node.location(), kind: "target" });
            }
        }
        _ => {}
    }
    for child in node.children() {
        walk_component_refs(&child, parsing, target_name, out);
    }
}
