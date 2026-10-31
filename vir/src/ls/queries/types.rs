//! Type rendering shared by the queries (and the LSP code actions).

use bstr::ByteSlice as _;

use virdant::db::Db;
use virdant::package::PackageId;
use virdant::types::Type;

/// Renders a `Type` as Virdant source syntax, qualifying user-defined
/// types with their defining package when that package is neither the
/// destination module's own package nor already imported there.
/// (Shared with the LSP code actions.)
pub fn type_to_source(db: &Db, module_package: PackageId, typ: &Type) -> String {
    match typ {
        Type::Bit => "Bit".to_string(),
        Type::Clock => "Clock".to_string(),
        Type::Reset => "Reset".to_string(),
        Type::Word(width) => format!("Word[{width}]"),
        Type::Param(_) => "?".to_string(),
        Type::Usual(symbol_id, args) => {
            let symbol = db.get_symboltable().symbol(*symbol_id);
            let def_package = symbol.location.package();
            let name = symbol.name.to_str_lossy().into_owned();
            let qualified = if def_package == module_package || def_package == db.get_packages().builtin() {
                name
            } else {
                let imported = db.get_package_analysis(module_package)
                    .imports()
                    .contains(&def_package);
                if imported {
                    name
                } else {
                    let pkg_name = db.get_packages().name(def_package).to_str_lossy().into_owned();
                    format!("{pkg_name}::{name}")
                }
            };
            if args.is_empty() {
                qualified
            } else {
                let args: Vec<String> = args
                    .iter()
                    .map(|arg| type_to_source(db, module_package, arg))
                    .collect();
                format!("{}[{}]", qualified, args.join(", "))
            }
        }
    }
}
