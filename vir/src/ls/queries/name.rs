//! Name resolution for the name-based queries
//! (definition / references / components by name).

use bstr::{BString, ByteSlice as _};

use virdant::analysis::symbols::Symbol;
use virdant::db::Db;

/// The result of resolving a name target: either a symbol, several
/// equally-named symbols, or none.
pub enum NameResolution {
    Found(Symbol),
    Ambiguous(Vec<Symbol>),
    NotFound,
}

/// Resolves a name target for the name-based queries: a fully-qualified
/// name (`pkg::Item` or `pkg::Item::component`) resolves directly in
/// the symbol table; a bare item name is searched across all non-builtin
/// packages and must match exactly one symbol.
pub fn resolve_name(db: &Db, target: &str) -> NameResolution {
    let symboltable = db.get_symboltable();
    let fqn = BString::from(target.as_bytes());
    if target.contains("::") {
        return match symboltable.resolve(fqn.as_bstr()) {
            Some(symbol) => NameResolution::Found(symbol.clone()),
            None => NameResolution::NotFound,
        };
    }
    // Bare name: search items whose name matches, excluding the builtin
    // library (builtin type names like `Word` are not reasonable
    // unqualified targets).
    let packages = db.get_packages();
    let builtin = packages.builtin();
    let name = fqn.as_bstr();
    let mut matches = vec![];
    for symbol in symboltable.items() {
        if symbol.name.as_bstr() == name && symbol.location.package() != builtin {
            matches.push(symbol);
        }
    }
    match matches.len() {
        0 => NameResolution::NotFound,
        1 => NameResolution::Found(matches.pop().unwrap()),
        _ => NameResolution::Ambiguous(matches),
    }
}
