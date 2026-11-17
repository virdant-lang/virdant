//! Defines `Domain`, the clock-domain lattice assigned to components and
//! expressions by the clock checker.

use std::cmp::Ordering;

use crate::analysis::component::ComponentId;

/// The clock a `Sync` domain is clocked by: a clock *class*, the
/// equivalence class of Clock-typed things under the relation generated
/// by the module's clock sources and its clock definitions.
///
/// A concrete class is named by its canonical source: the component id
/// of the incoming clock port, platform clock port, instance clock
/// output, or generating builtin that introduces the identity.  Two
/// Clock-typed components denote the same class exactly when they
/// resolve to the same canonical source; the clock checker's binding
/// and class resolution establish this.
///
/// An abstract clock variable (`Var`) is everything a class is except
/// sourced: a flexible module's clock variable `α`, or the unbound
/// clock of an instance clock port.  It behaves as an ordinary clock
/// class inside its own view — in particular it is incomparable with
/// every concrete class — and it is substituted (or left unbound, which
/// the parent's view treats as `Async`) at the instantiation site.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Clock {
    /// A concrete clock class, named by its canonical source component
    /// of the enclosing module's `ComponentAnalysis`.
    Class(ComponentId),
    /// An abstract clock variable: never sourced, never equal to a
    /// concrete class.
    Var(u32),
}

/// The clock domain of a component or expression.
///
/// The domains form a lattice:
///
/// ```text
///           Async
///          /  |  \
///    Sync(k1) Sync(k2) ...
///          \  |  /
///         Constant
/// ```
///
/// A domain may be safely *upcast* toward the top: a `Constant` may be
/// used where any domain is expected, and a clocked signal may be used
/// where an async one is expected.  Downcasting is never allowed, and
/// two distinct clock classes (`Sync(k1)`, `Sync(k2)` with `k1 != k2`)
/// are incomparable: neither may be used where the other is expected.
///
/// Clock-typed components are clocked by the very class they denote:
/// a clock is a value on its own clock.  Domains never cross module
/// boundaries: a submodule's internal `Sync` domains are mapped onto
/// the parent's clocks by the definitions that drive the submodule's
/// clock ports.  To render a domain for a diagnostic, resolve the
/// class's canonical source path through the enclosing
/// `ComponentAnalysis`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Domain {
    /// Clocked by the clock class `k`.
    Sync(Clock),
    /// Not bound to any clock.
    Async,
    /// Independent of every clock: word literals, enumerants, struct
    /// and union values built entirely from constants, and any
    /// expression combining only such values.
    Constant,
}

impl Clock {
    /// The canonical source component of a concrete class, or `None`
    /// for an abstract clock variable.
    pub fn class(&self) -> Option<ComponentId> {
        match self {
            Clock::Class(component_id) => Some(*component_id),
            Clock::Var(_) => None,
        }
    }

    /// Whether this clock is an abstract clock variable.
    pub fn is_var(&self) -> bool {
        matches!(self, Clock::Var(_))
    }
}

impl Domain {
    /// Constructs the domain clocked by the concrete class whose
    /// canonical source is the component `clock`.
    pub fn sync(clock: ComponentId) -> Domain {
        Domain::Sync(Clock::Class(clock))
    }

    /// Constructs the domain clocked by the abstract clock variable
    /// `var`.
    pub fn sync_var(var: u32) -> Domain {
        Domain::Sync(Clock::Var(var))
    }

    /// The concrete clock class this domain is bound to, if any.
    pub fn clock(&self) -> Option<ComponentId> {
        match self {
            Domain::Sync(clock) => clock.class(),
            Domain::Async | Domain::Constant => None,
        }
    }

    /// Whether this domain may be used where `target` is expected:
    /// the lattice order `Constant <= Sync(k) <= Async`, with distinct
    /// clock classes incomparable.  An abstract clock variable is an
    /// ordinary class: comparable to itself alone.
    pub fn upcasts_to(&self, target: &Domain) -> bool {
        match (self, target) {
            (Domain::Constant, _) => true,
            // An unbound abstract clock variable accepts anything, in
            // both directions: no concrete clock has committed it, so
            // there is nothing to conflict with.  (Drivers into it are
            // candidate substitutions, recorded by the pinning pass;
            // reads of it unify with the reader's clock.)
            (Domain::Sync(Clock::Var(_)), _) => true,
            (_, Domain::Sync(Clock::Var(_))) => true,
            (Domain::Sync(clock1), Domain::Sync(clock2)) => clock1 == clock2,
            (Domain::Sync(_), Domain::Async) => true,
            (Domain::Async, Domain::Async) => true,
            _ => false,
        }
    }

    /// The least upper bound of two domains in the lattice: the least
    /// domain both may be upcast to.  The join of two distinct clock
    /// classes is `Async`.
    pub fn lub(a: &Domain, b: &Domain) -> Domain {
        match (a, b) {
            _ if a == b => *a,
            (Domain::Constant, other) | (other, Domain::Constant) => *other,
            (Domain::Sync(Clock::Var(_)), other) | (other, Domain::Sync(Clock::Var(_))) => *other,
            (Domain::Sync(_), Domain::Async) | (Domain::Async, Domain::Sync(_)) => Domain::Async,
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
            (Domain::Sync(Clock::Var(_)), other) | (other, Domain::Sync(Clock::Var(_))) => *other,
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
