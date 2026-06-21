use crate::analysis::symbols::SymbolId;
use crate::common::Width;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bit,
    Clock,
    Reset,
    Word(Width),
    Usual(SymbolId), // TODO rename this
    Valid(Box<Type>),
}

/// Represents the clock domain of a value.
///
/// Every value in Virdant belongs to exactly one clock domain.
/// The domain determines when the value is valid and can be sampled.
/// Type and ClockDomain are orthogonal: a `Bit` can exist in any domain.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ClockDomain {
    /// Value is not synchronized to any clock.
    /// May change at any time unrelated to clock edges.
    /// Cannot be used in sequential logic without synchronization.
    Async,

    /// Value is synchronized to a specific clock signal.
    /// The SymbolId identifies the Clock-typed component.
    OnClock(SymbolId),

    /// Domain could not be inferred.
    /// Used internally during type checking.
    /// Invalid as a final result - produces an error.
    Unknown,
}

impl ClockDomain {
    /// Returns true if this is an async domain.
    pub fn is_async(&self) -> bool {
        matches!(self, ClockDomain::Async)
    }

    /// Returns true if this is a clocked domain.
    pub fn is_clocked(&self) -> bool {
        matches!(self, ClockDomain::OnClock(_))
    }

    /// Returns the clock symbol if this is a clocked domain.
    pub fn clock_symbol(&self) -> Option<SymbolId> {
        match self {
            ClockDomain::OnClock(id) => Some(*id),
            _ => None,
        }
    }

    /// Checks if two domains are compatible.
    /// Unknown never matches anything (including itself),
    /// so that inference keeps propagating constraints.
    pub fn matches(&self, other: &ClockDomain) -> bool {
        match (self, other) {
            (ClockDomain::Async, ClockDomain::Async) => true,
            (ClockDomain::OnClock(a), ClockDomain::OnClock(b)) => a == b,
            _ => false,
        }
    }
}

impl std::fmt::Display for ClockDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClockDomain::Async => write!(f, "async"),
            ClockDomain::OnClock(id) => write!(f, "on clock({:?})", id),
            ClockDomain::Unknown => write!(f, "<unknown domain>"),
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
            Type::Valid(inner) => write!(f, "Valid[{inner}]"),
            Type::Usual(_symbol_id) => write!(f, "{self:?}"),
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
            Type::Valid(inner) => write!(f, "Valid({inner:?})"),
            Type::Usual(symbol_id) => write!(f, "Usual({symbol_id:?})"),
        }
    }
}
