//! Generic-to-mono type substitution: `substitute` replaces every
//! `Type::Param` in a type with the corresponding type argument.  This
//! is the single shared substitution function; no consumer of
//! `Type::Usual` member types hand-rolls its own.

use super::typ::Type;

/// Substitute every `Param(i)` in `typ` with `args[i as usize]`.
///
/// `args` are the type arguments of the generic type def whose member
/// types are being substituted, in declaration order.  A `Param` whose
/// index is outside `args` (which can only happen on malformed input)
/// is left as-is.
///
/// Recurses through `Usual` argument lists, so a fully-substituted type
/// contains no `Param` anywhere, provided each argument itself is
/// mono (see `Type::is_mono`).
pub fn substitute(typ: &Type, args: &[Type]) -> Type {
    match typ {
        Type::Bit | Type::Clock | Type::Reset | Type::Word(_) => typ.clone(),
        Type::Param(id) => {
            let index = id.0 as usize;
            args.get(index).cloned().unwrap_or_else(|| typ.clone())
        }
        Type::Usual(symbol_id, inner_args) => Type::Usual(
            *symbol_id,
            inner_args.iter().map(|arg| substitute(arg, args)).collect(),
        ),
    }
}
