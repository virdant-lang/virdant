//! The definition query: resolving what a location (or a reference)
//! points to.

use bstr::{BString, ByteSlice as _};

use virdant::analysis::Location;
use virdant::analysis::symbols::Symbol;
use virdant::common::source::LineCol;
use virdant::db::Db;
use virdant::package::PackageId;
use virdant::syntax::ast::AstNodeId;
use virdant::syntax::parsing::Parsing;
use virdant::syntax::payload::AstNodePayload;

/// Resolves the reference target of a node: the `Location` the node
/// refers to.  Handles `Ofness` (item references, type positions,
/// platform `for` clauses), bare `Path` occurrences, `ExprReference`,
/// and `Type` nodes.  Returns `None` for non-reference nodes.  Must
/// not call back into [`resolve_definition_at`] or
/// [`resolve_symbol_at`], which both delegate here.
pub(super) fn resolve_reference_target(db: &Db, parsing: &Parsing, package: PackageId, node_id: AstNodeId) -> Option<Location> {
    let node = parsing.ast_node(node_id);
    match node.payload() {
        AstNodePayload::Ofness(ofness) => {
            // Resolve the item this Ofness names (e.g. `Gcd` in
            // `mod gcd of Gcd`, a type position, or a platform `for`).
            let symboltable = db.get_symboltable();
            let packages = db.get_packages();
            let item_pkg: Option<PackageId> = match ofness.package {
                Some(pkg) => packages.id(parsing.string(pkg)),
                None => Some(package),
            };
            let item_name = parsing.string(ofness.name);
            item_pkg.and_then(|pkg| symboltable.resolve_item(item_name, pkg))
                .map(|sym| sym.location())
        }
        AstNodePayload::Path(path_payload) => {
            // A bare path: the driver target / reference spelling,
            // resolved in the enclosing module's component analysis.
            let item = enclosing_item(db, parsing, package, node_id)?;
            let component_analysis = db.get_component_analysis(item.id);
            let path_str = parsing.string(path_payload.path);
            component_analysis.resolve(path_str)
                .map(|comp| comp.location())
        }
        AstNodePayload::ExprReference => {
            // ExprReference wraps a Path child; node.path() extracts its
            // interned string.
            let Some(path_interned) = node.path() else { return None; };
            let item = enclosing_item(db, parsing, package, node_id)?;
            let component_analysis = db.get_component_analysis(item.id);
            let path_str = parsing.string(path_interned);
            component_analysis.resolve(path_str)
                .map(|comp| comp.location())
        }
        AstNodePayload::Type(_) => {
            // A Type node's first child is always an Ofness carrying the
            // type name.
            let ofness_node = node.child(0);
            let AstNodePayload::Ofness(ofness) = ofness_node.payload() else { return None; };
            let symboltable = db.get_symboltable();
            let packages = db.get_packages();
            let item_pkg: Option<PackageId> = match ofness.package {
                Some(pkg) => packages.id(parsing.string(pkg)),
                None => Some(package),
            };
            let item_name = parsing.string(ofness.name);
            item_pkg.and_then(|pkg| symboltable.resolve_item(item_name, pkg))
                .map(|sym| sym.location())
        }
        _ => None,
    }
}

/// The symbol declared at a node: an item (its own symbol) or a
/// component declaration (via the `pkg::Item::name` fqn).  Returns
/// `None` for non-declaration nodes.
pub(super) fn declaration_symbol_at(db: &Db, parsing: &Parsing, package: PackageId, node_id: AstNodeId) -> Option<Symbol> {
    let node = parsing.ast_node(node_id);
    let symboltable = db.get_symboltable();

    if node.is_item() {
        let name = parsing.string(node.name()?);
        return symboltable.resolve_item_in_package(name, package).cloned();
    }

    // A component declaration's symbol has fqn `pkg::Item::name`.
    if let AstNodePayload::Component(component) = node.payload() {
        if let Some(item) = enclosing_item(db, parsing, package, node_id) {
            let item_name = item.name().to_str_lossy();
            let fqn: BString = format!(
                "{}::{}::{}",
                db.get_packages().name(package).to_str_lossy(),
                item_name,
                parsing.string(component.name).to_str_lossy(),
            ).into();
            if let Some(symbol) = symboltable.resolve(fqn.as_bstr()) {
                return Some(symbol.clone());
            }
        }
    }

    None
}

/// Resolves the definition target of the AST node at `linecol` in
/// `package`: type/ofness references resolve to the named item's
/// symbol, component references (`Path`/`ExprReference`) resolve
/// through the enclosing module's component analysis.  Returns the
/// target's `Location` (package + AST node) when resolved.  This is the
/// single core both the LSP `goto_definition` handler and the CLI
/// `definition` query call.
pub fn resolve_definition_at(db: &Db, package: PackageId, linecol: LineCol) -> Option<Location> {
    let parsing = db.get_parsing(package);
    let node_id = parsing.at(linecol)?;
    // A reference location resolves to what it refers to; otherwise the
    // location may be a declaration itself (an item or component
    // definition), in which case its own location is the definition.
    resolve_reference_target(db, &parsing, package, node_id)
        .or_else(|| declaration_symbol_at(db, &parsing, package, node_id).map(|symbol| symbol.location()))
}

/// Walks up the AST from `node_id` to the enclosing item node and
/// resolves its name to the item's symbol in `package`.
pub(super) fn enclosing_item(db: &Db, parsing: &Parsing, package: PackageId, node_id: AstNodeId) -> Option<Symbol> {
    let mut current_id = node_id;
    loop {
        let n = parsing.ast_node(current_id);
        if n.is_item() { break; }
        match n.parent().map(|p| p.id()) {
            Some(id) => current_id = id,
            None => return None,
        }
    }
    let current = parsing.ast_node(current_id);
    let item_name = parsing.string(current.name().unwrap());
    db.get_symboltable().resolve_item_in_package(item_name, package).cloned()
}
