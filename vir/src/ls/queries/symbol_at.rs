//! Symbol resolution at a location.

use virdant::analysis::symbols::Symbol;
use virdant::common::source::LineCol;
use virdant::db::Db;
use virdant::package::PackageId;

use super::definition::{declaration_symbol_at, resolve_reference_target};

/// The symbol at a location: the item declared there, the component
/// declared there, or — for a reference location — the item/component
/// the reference resolves to.  Best-effort: returns `None` when no
/// symbol can be attributed to the location.
pub fn resolve_symbol_at(db: &Db, package: PackageId, linecol: LineCol) -> Option<Symbol> {
    let parsing = db.get_parsing(package);
    let node_id = parsing.at(linecol)?;
    let symboltable = db.get_symboltable();

    // A declaration resolves to its own symbol.
    if let Some(symbol) = declaration_symbol_at(db, &parsing, package, node_id) {
        return Some(symbol);
    }

    // A reference: resolve what it points to, then find the symbol
    // declared at the target location.
    let target = resolve_reference_target(db, &parsing, package, node_id)?;
    symboltable.symbols().into_iter().find(|symbol| symbol.location() == target)
}
