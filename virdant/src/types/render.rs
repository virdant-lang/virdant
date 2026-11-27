//! Rendering a `Type` back to Virdant-source-like text for *humans*:
//! mono-module origin comments and mismatched-instantiation
//! diagnostics.  Not used for name mangling (which hashes the `Type`
//! values instead, since a string rendering may not be unique in the
//! future).

use bstr::ByteSlice;

use crate::analysis::symbols::SymbolId;
use crate::db::Db;
use crate::package::PackageId;
use crate::types::typ::{ParamId, Type};
use crate::types::typedef::TypeParam;

/// Render a `Usual` type's item name the way it would be referenced
/// in the file of package `current`: the bare name when the item is
/// builtin (implicit scope) or defined in `current` itself, and the
/// fully-qualified name (`package::Name`) for foreign types.
fn render_item_name(db: &Db, symbol_id: SymbolId, current: Option<PackageId>) -> String {
    let symbol = db.get_symboltable().symbol(symbol_id);
    let packages = db.get_packages();
    let pkg = symbol.location.package();
    if pkg == packages.builtin() || Some(pkg) == current {
        symbol.name().to_str_lossy().into_owned()
    } else {
        symbol.fqn().to_str_lossy().into_owned()
    }
}

/// Render `def_symbol`'s name with its type arguments,
/// e.g. `Pipe[Word[8]]` or `Fifo`.  `params` (the def's declared
/// generic parameters) give `Param` arguments their source names.
pub fn render_mono_item(
    db: &Db,
    def_symbol: SymbolId,
    params: &[TypeParam],
    args: &[Type],
) -> String {
    render_mono_item_inner(db, None, def_symbol, params, args)
}

/// Like [`render_mono_item`], but elides the bare name for items of
/// `current` as well as for builtins, matching how the source of
/// `current` would reference them.
pub fn render_mono_item_in_package(
    db: &Db,
    current: PackageId,
    def_symbol: SymbolId,
    params: &[TypeParam],
    args: &[Type],
) -> String {
    render_mono_item_inner(db, Some(current), def_symbol, params, args)
}

/// Shared core for the `render_mono_item*` wrappers.
fn render_mono_item_inner(
    db: &Db,
    current: Option<PackageId>,
    def_symbol: SymbolId,
    params: &[TypeParam],
    args: &[Type],
) -> String {
    let name = render_item_name(db, def_symbol, current);
    if args.is_empty() {
        name
    } else {
        let rendered_args: Vec<String> = args
            .iter()
            .map(|arg| render_type_inner(db, current, arg, params))
            .collect();
        format!("{name}[{}]", rendered_args.join(", "))
    }
}

/// Render a type in Virdant-source-like syntax, without a parameter
/// context (`Param` renders as `Param`).
pub fn render_type(db: &Db, typ: &Type) -> String {
    render_type_inner(db, None, typ, &[])
}

/// Render a type the way it would be referenced in the file of
/// package `current`: builtin types and types of `current` itself
/// by bare name, foreign types by fully-qualified name.
pub fn render_type_in_package(db: &Db, current: PackageId, typ: &Type) -> String {
    render_type_inner(db, Some(current), typ, &[])
}

/// Render a type in Virdant-source-like syntax; a `Param(id)` renders
/// as the parameter's declared name when `params` is the declaring
/// def's parameter list.
pub fn render_type_with(db: &Db, typ: &Type, params: &[TypeParam]) -> String {
    render_type_inner(db, None, typ, params)
}

/// Shared core for the `render_type*` wrappers: renders a type with
/// `current` = the package whose file the text is rendered for (elides
/// that package's own names to bare names, and builtins to bare names),
/// and `params` = the generic-parameter list that names `Type::Param`s.
fn render_type_inner(
    db: &Db,
    current: Option<PackageId>,
    typ: &Type,
    params: &[TypeParam],
) -> String {
    match typ {
        Type::Bit => "Bit".to_string(),
        Type::Clock => "Clock".to_string(),
        Type::Reset => "Reset".to_string(),
        Type::Word(width) => format!("Word[{width}]"),
        Type::Param(param_id) => render_param(*param_id, params),
        Type::Usual(symbol_id, args) => {
            let name = render_item_name(db, *symbol_id, current);
            if args.is_empty() {
                name
            } else {
                let rendered_args: Vec<String> = args
                    .iter()
                    .map(|arg| render_type_inner(db, current, arg, params))
                    .collect();
                format!("{name}[{}]", rendered_args.join(", "))
            }
        }
    }
}

/// Render a type in source-like syntax with *every* type name spelled
/// as its fully-qualified name — for machine-readable output where a
/// consumer must resolve each name unambiguously.  The builtin
/// primitives are qualified too (`builtin::Word[8]`, `builtin::Bit`),
/// as are builtin item types (`builtin::Valid[Word[8]]`).
pub fn render_type_fqn(db: &Db, typ: &Type) -> String {
    let packages = db.get_packages();
    let prefix = format!("{}::", packages.name(packages.builtin()).to_str_lossy());
    match typ {
        Type::Bit => format!("{prefix}Bit"),
        Type::Clock => format!("{prefix}Clock"),
        Type::Reset => format!("{prefix}Reset"),
        Type::Word(width) => format!("{prefix}Word[{width}]"),
        Type::Param(param_id) => render_param(*param_id, &[]),
        Type::Usual(symbol_id, args) => {
            let name = db
                .get_symboltable()
                .symbol(*symbol_id)
                .fqn()
                .to_str_lossy()
                .into_owned();
            if args.is_empty() {
                name
            } else {
                let rendered_args: Vec<String> = args
                    .iter()
                    .map(|arg| render_type_fqn(db, arg))
                    .collect();
                format!("{name}[{}]", rendered_args.join(", "))
            }
        }
    }
}

fn render_param(param_id: ParamId, params: &[TypeParam]) -> String {
    params
        .get(param_id.0 as usize)
        .map(|param| param.name.to_str_lossy().into_owned())
        .unwrap_or_else(|| format!("Param({})", param_id.0))
}
