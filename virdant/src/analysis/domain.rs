//! Defines `Domain`, the clock-domain lattice assigned to components and
//! expressions by the clock checker.

use std::cmp::Ordering;

use crate::analysis::component::ComponentId;

/// The clock domain of a component or expression.
///
/// The domains form a lattice:
///
/// ```text
///           Async
///          /  |  \
///    Sync(c1) Sync(c2) ...
///          \  |  /
///         Constant
/// ```
///
/// A domain may be safely *upcast* toward the top: a `Constant` may be
/// used where any domain is expected, and a clocked signal may be used
/// where an async one is expected.  Downcasting is never allowed, and
/// two distinct clock domains (`Sync(c1)`, `Sync(c2)` with `c1 != c2`)
/// are incomparable: neither may be used where the other is expected.
///
/// A `Sync` domain names its clock by [`ComponentId`] — the clock
/// component of the enclosing module's `ComponentAnalysis` (the
/// module's clock port, an inward socket channel, a submodule
/// instance's clock output, etc.).  Domains never cross module
/// boundaries: a submodule's internal `Sync` domains are mapped onto
/// the parent's clocks by the connection that drives the submodule's
/// clock port.  To render a domain for a diagnostic, resolve the clock
/// id's path through the enclosing `ComponentAnalysis`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Domain {
    /// Clocked by the component id.
    Sync(ComponentId),
    /// Not bound to any clock.
    Async,
    /// Independent of every clock: word literals, enumerants, struct
    /// and union values built entirely from constants, and any
    /// expression combining only such values.
    Constant,
}

impl Domain {
    /// Constructs the domain clocked by the component `clock`.
    pub fn sync(clock: ComponentId) -> Domain {
        Domain::Sync(clock)
    }

    /// The clock component this domain is bound to, if any.
    pub fn clock(&self) -> Option<ComponentId> {
        match self {
            Domain::Sync(clock) => Some(*clock),
            Domain::Async | Domain::Constant => None,
        }
    }

    /// Whether this domain may be used where `target` is expected:
    /// the lattice order `Constant <= Sync(c) <= Async`, with distinct
    /// clock domains incomparable.
    pub fn upcasts_to(&self, target: &Domain) -> bool {
        match (self, target) {
            (Domain::Constant, _) => true,
            (Domain::Sync(clock1), Domain::Sync(clock2)) => clock1 == clock2,
            (Domain::Sync(_), Domain::Async) => true,
            (Domain::Async, Domain::Async) => true,
            _ => false,
        }
    }

    /// The least upper bound of two domains in the lattice: the least
    /// domain both may be upcast to.  The join of two distinct clock
    /// domains is `Async`.
    pub fn lub(a: &Domain, b: &Domain) -> Domain {
        match (a, b) {
            _ if a == b => *a,
            (Domain::Constant, other) | (other, Domain::Constant) => *other,
            _ => Domain::Async,
        }
    }

    /// The greatest lower bound of two domains in the lattice: the
    /// greatest domain that may be upcast to both.  The meet of two
    /// distinct clock domains is `Constant`.
    pub fn glb(a: &Domain, b: &Domain) -> Domain {
        match (a, b) {
            _ if a == b => *a,
            (Domain::Async, other) | (other, Domain::Async) => *other,
            _ => Domain::Constant,
        }
    }
}

impl PartialOrd for Domain {
    fn partial_cmp(&self, other: &Domain) -> Option<Ordering> {
        match (self.upcasts_to(other), other.upcasts_to(self)) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Less),
            (false, true) => Some(Ordering::Greater),
            (false, false) => None,
        }
    }
}
