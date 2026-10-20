//! Defines the core `Type` enum representing Virdant's type system:
//! `Bit`, `Clock`, `Reset`, `Word`, `Usual` (user-defined types, with
//! their type arguments), and `Param` (a reference to a generic
//! parameter), with `Display` and `Debug` implementations.

use crate::analysis::symbols::SymbolId;
use crate::common::Width;

/// Identifies a generic parameter of a generic type definition by its
/// (0-based) position in the definition's parameter declaration list.
///
/// A `Type::Param` is only legal inside the analysis of the generic
/// type def which declares the parameter (its members are stored as
/// written); mono artifacts (codegen, simulation, widths) must never
/// contain one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParamId(pub u32);

/// The kind of a generic parameter declared by a generic definition
/// (`[T : Type]` or `[n : Nat]`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// The parameter stands for a type.
    Type,
    /// The parameter stands for a natural number (a width).
    Nat,
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Type => write!(f, "Type"),
            Kind::Nat => write!(f, "Nat"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bit,
    Clock,
    Reset,
    Word(Width),
    /// A user-defined type, with the type arguments at this use site.
    /// The arguments are already fully-resolved semantic types; a
    /// numeric argument written as a literal (`Pair[8, Bit]`) becomes
    /// `Word(8)`.
    Usual(SymbolId, Vec<Type>),
    /// A reference to a generic parameter of the generic type def
    /// being analyzed.  Only legal in generic analyses, never in mono
    /// artifacts.
    Param(ParamId),
}

impl Type {
    /// True when the type contains a `Param` anywhere inside it, i.e.
    /// it is generic rather than mono.  Mono artifacts must never
    /// contain such a type.
    pub fn is_mono(&self) -> bool {
        match self {
            Type::Bit | Type::Clock | Type::Reset | Type::Word(_) => true,
            Type::Param(_) => false,
            Type::Usual(_, args) => args.iter().all(|arg| arg.is_mono()),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bit => write!(f, "Bit"),
            Type::Clock => write!(f, "Clock"),
            Type::Reset => write!(f, "Reset"),
            Type::Word(n) => write!(f, "Word[{n}]"),
            Type::Param(id) => write!(f, "Param[{id:?}]"),
            Type::Usual(_symbol_id, _args) => write!(f, "{self:?}"),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bit => write!(f, "Bit"),
            Type::Clock => write!(f, "Clock"),
            Type::Reset => write!(f, "Reset"),
            Type::Word(n) => write!(f, "Word[{n}]"),
            Type::Param(id) => write!(f, "Param({id:?})"),
            Type::Usual(symbol_id, args) => {
                if args.is_empty() {
                    write!(f, "Usual({symbol_id:?})")
                } else {
                    write!(f, "Usual({symbol_id:?}, {args:?})")
                }
            }
        }
    }
}
