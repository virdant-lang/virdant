//! The symbols query: listing the symbol table's items.

use bstr::ByteSlice as _;

use virdant::analysis::Location;
use virdant::analysis::symbols::SymbolKind;
use virdant::db::Db;
use virdant::package::PackageId;

/// One item from the symbol table, as served by the `symbols` query.
pub struct SymbolEntry {
    pub name: String,
    pub kind: &'static str,
    pub package: PackageId,
    pub location: Location,
}

/// The canonical spelling of an item kind (`SymbolKind` restricted to
/// item kinds).
pub fn symbol_kind_str(kind: &SymbolKind) -> &'static str {
    match kind {
        SymbolKind::ModDef => "moddef",
        SymbolKind::UnionDef => "uniondef",
        SymbolKind::StructDef => "structdef",
        SymbolKind::EnumDef => "enumdef",
        SymbolKind::BuiltinDef => "builtindef",
        SymbolKind::FnDef => "fndef",
        SymbolKind::SocketDef => "socketdef",
        SymbolKind::Platform => "platform",
        _ => "item",
    }
}

/// The items in `package` (or all packages when `package` is `None`).
pub fn symbols(db: &Db, package: Option<PackageId>) -> Vec<SymbolEntry> {
    db.get_symboltable()
        .items()
        .into_iter()
        .filter(|symbol| package.map_or(true, |p| symbol.location.package() == p))
        .map(|symbol| SymbolEntry {
            name: symbol.name.to_str_lossy().into_owned(),
            kind: symbol_kind_str(&symbol.kind),
            package: symbol.location.package(),
            location: symbol.location,
        })
        .collect()
}
