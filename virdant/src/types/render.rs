//! Rendering a `Type` back to Virdant-source-like text for *humans*:
//! mono-module origin comments and mismatched-instantiation
//! diagnostics.  Not used for name mangling (which hashes the `Type`
//! values instead, since a string rendering may not be unique in the
//! future).

use bstr::ByteSlice;

use crate::analysis::symbols::SymbolId;
use crate::db::Db;
use crate::types::typ::{ParamId, Type};
use crate::types::typedef::TypeParam;

/// Render a `Usual` type's item name: the fully-qualified name,
/// except that the `builtin` package's implicit scope is elided
/// (`Bit`, not `builtin::Bit`), matching how the source writes it.
fn render_item_name(db: &Db, symbol_id: SymbolId) -> String {
    let symbol = db.get_symboltable().symbol(symbol_id);
    let packages = db.get_packages();
    if symbol.location.package() == packages.builtin() {
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
    let name = render_item_name(db, def_symbol);
    if args.is_empty() {
        name
    } else {
        let rendered_args: Vec<String> = args
            .iter()
            .map(|arg| render_type_with(db, arg, params))
            .collect();
        format!("{name}[{}]", rendered_args.join(", "))
    }
}

/// Render a type in Virdant-source-like syntax, without a parameter
/// context (`Param` renders as `Param`).
pub fn render_type(db: &Db, typ: &Type) -> String {
    render_type_with(db, typ, &[])
}

/// Render a type in Virdant-source-like syntax; a `Param(id)` renders
/// as the parameter's declared name when `params` is the declaring
/// def's parameter list.
pub fn render_type_with(db: &Db, typ: &Type, params: &[TypeParam]) -> String {
    match typ {
        Type::Bit => "Bit".to_string(),
        Type::Clock => "Clock".to_string(),
        Type::Reset => "Reset".to_string(),
        Type::Word(width) => format!("Word[{width}]"),
        Type::Param(param_id) => render_param(*param_id, params),
        Type::Usual(symbol_id, args) => {
            let name = render_item_name(db, *symbol_id);
            if args.is_empty() {
                name
            } else {
                let rendered_args: Vec<String> = args
                    .iter()
                    .map(|arg| render_type_with(db, arg, params))
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
