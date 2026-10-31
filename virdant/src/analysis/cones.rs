//! Computes `Cones`, the combinational cone graphs of an elaborated
//! design, for every register and every top-level port of the design.
//!
//! A *cone* of a signal is the set of endpoints connected to it through
//! purely combinational logic: the **forward cone** of `R` is the set of
//! endpoints reachable from `R`'s output without passing through another
//! register; the **backward cone** is the set of endpoints feeding `R`'s
//! input without passing through another register.  Registers act as cone
//! endpoints because a register's output reflects its input from a
//! previous cycle, not the current one.
//! The top module's own ports are also cone endpoints, since they sit at
//! the design boundary: an `incoming` port terminates a backward cone
//! (nothing in the design drives it; the world does) and an `outgoing`
//! port that nothing reads internally terminates a forward cone (it
//! drives the world), so every cone grounds out in a real source or
//! sink.  For a platform-bound top these ports are exactly the
//! platform's pins.
//!
//! The edges come from the same dependency map the simulator uses
//! (`analysis::deps`), including socket-connection aliases (`:=:`).

use std::sync::Arc;

use bstr::ByteSlice;
use indexmap::{IndexMap, IndexSet};

use crate::analysis::deps::build_dependencies;
use crate::analysis::elaboration::SignalId;
use crate::analysis::symbols::SymbolId;
use crate::common::ComponentKind;
use crate::db::Builder;

#[derive(Debug)]
pub struct Cones {
    /// Every signal with cones, in elaboration order: each register of
    /// the design, plus each top-level port of the top module.
    signals: IndexSet<SignalId>,
    /// For each signal, the endpoints reachable from its output through
    /// purely combinational logic.
    forward: IndexMap<SignalId, IndexSet<SignalId>>,
    /// For each signal, the endpoints feeding its input through purely
    /// combinational logic.
    backward: IndexMap<SignalId, IndexSet<SignalId>>,
    /// The registers among `signals`.
    registers: IndexSet<SignalId>,
    /// The top module's `incoming` ports: natural sources at the design
    /// boundary (the world drives them; for a platform-bound top these
    /// are the platform's input pins).
    boundary_sources: IndexSet<SignalId>,
    /// The top module's `outgoing` ports that nothing in the design
    /// reads: natural sinks at the design boundary (they drive the
    /// world; for a platform-bound top these are the platform's output
    /// pins).
    boundary_sinks: IndexSet<SignalId>,
}

impl Cones {
    /// Every signal with cones, in elaboration order: registers and the
    /// top module's ports.
    pub fn signals(&self) -> &IndexSet<SignalId> {
        &self.signals
    }

    /// The registers these cones cover, in elaboration order.
    pub fn registers(&self) -> impl Iterator<Item = SignalId> + '_ {
        self.registers.iter().copied()
    }

    /// Registers whose inputs are reachable from `reg`'s output through
    /// combinational logic.  `None` if `reg` is not a register of the
    /// elaborated design.
    pub fn forward(&self, reg: SignalId) -> Option<&IndexSet<SignalId>> {
        self.forward.get(&reg)
    }

    /// Registers whose outputs feed `reg`'s input through combinational
    /// logic.  `None` if `reg` is not a register of the elaborated design.
    pub fn backward(&self, reg: SignalId) -> Option<&IndexSet<SignalId>> {
        self.backward.get(&reg)
    }

    /// The top module's `incoming` ports (natural boundary sources).
    pub fn boundary_sources(&self) -> &IndexSet<SignalId> {
        &self.boundary_sources
    }

    /// The top module's `outgoing` ports that nothing reads internally
    /// (natural boundary sinks).
    pub fn boundary_sinks(&self) -> &IndexSet<SignalId> {
        &self.boundary_sinks
    }

    /// Whether `signal` is one of the registers covered by these cones.
    pub fn is_register(&self, signal: SignalId) -> bool {
        self.registers.contains(&signal)
    }
}

pub(crate) fn build_cones(builder: &mut Builder, top: SymbolId) -> Arc<Cones> {
    let elaboration = builder.get_elaboration(top);

    // Dataflow edges of the elaborated design, with socket-connection
    // aliases (`:=:`) added: a sink channel combinationally copies its
    // source peer's value.
    let mut deps = build_dependencies(builder, &elaboration);
    for component in elaboration.components() {
        if let Some(source) = component.alias() {
            deps.entry(component.id()).or_default().insert(source);
        }
    }

    // Invert into fanout: `fanout[p]` is the set of components that read `p`.
    let mut fanout: IndexMap<SignalId, IndexSet<SignalId>> = IndexMap::new();
    for (reader, read_set) in &deps {
        for &read in read_set {
            fanout.entry(read).or_default().insert(*reader);
        }
    }

    let registers: IndexSet<SignalId> = elaboration
        .components()
        .iter()
        .filter(|c| c.is_reg())
        .map(|c| c.id())
        .collect();

    // The signals cones are computed for: every register, plus every
    // top-level port of the top module (a port of the design is itself a
    // thing one wants cones for: what does `led_red` depend on?  what
    // does `uart_rx` drive?).  Every elaborated path starts with the top
    // module's fixed "top" prefix, so a component is top-level exactly
    // when its path has a single dot.  (`OutgoingReg` ports are already
    // registers.)
    let is_top_level = |path: &bstr::BStr| {
        let Some(last_dot) = path.rfind_byte(b'.') else { return false };
        path[..last_dot].rfind_byte(b'.').is_none()
    };
    let signals: IndexSet<SignalId> = elaboration
        .components()
        .iter()
        .filter(|c| {
            c.is_reg()
                || (is_top_level(c.path().as_bstr())
                    && matches!(
                        c.component_kind(),
                        ComponentKind::Incoming
                            | ComponentKind::Outgoing
                            | ComponentKind::OutgoingWire
                    ))
        })
        .map(|c| c.id())
        .collect();

    // Boundary endpoints: the top module's own ports sit at the design
    // boundary.  Every elaborated path starts with the top module's
    // fixed "top" prefix, so a component is top-level exactly when its
    // path has a single dot.  An `incoming` port is a source (nothing
    // in the design drives it); an `outgoing` port is a sink only when
    // nothing in the design reads it — one that is read internally is
    // an ordinary pass-through node, and stopping there would cut
    // paths that continue through it.
    let mut boundary_sources: IndexSet<SignalId> = IndexSet::new();
    let mut boundary_sinks: IndexSet<SignalId> = IndexSet::new();
    for component in elaboration.components() {
        let path = component.path();
        let Some(last_dot) = path.rfind_byte(b'.') else { continue };
        if path[..last_dot].rfind_byte(b'.').is_some() {
            continue;
        }
        match component.component_kind() {
            ComponentKind::Incoming if component.driver().is_none() => {
                boundary_sources.insert(component.id());
            }
            ComponentKind::Outgoing | ComponentKind::OutgoingWire
                if !component.is_reg()
                    && fanout.get(&component.id()).map_or(true, IndexSet::is_empty) =>
            {
                boundary_sinks.insert(component.id());
            }
            _ => {}
        }
    }

    let mut forward_endpoints = registers.clone();
    forward_endpoints.extend(boundary_sinks.iter().copied());
    let mut backward_endpoints = registers.clone();
    backward_endpoints.extend(boundary_sources.iter().copied());

    let mut forward: IndexMap<SignalId, IndexSet<SignalId>> = IndexMap::new();
    let mut backward: IndexMap<SignalId, IndexSet<SignalId>> = IndexMap::new();
    for signal in &signals {
        forward.insert(*signal, walk(*signal, &fanout, &forward_endpoints));
        backward.insert(*signal, walk(*signal, &deps, &backward_endpoints));
    }

    Arc::new(Cones {
        signals,
        forward,
        backward,
        registers,
        boundary_sources,
        boundary_sinks,
    })
}

/// Search from `start` over `edges` (fanout for forward cones, deps for
/// backward cones), treating `endpoints` (registers, plus the top
/// module's boundary ports on the relevant side) as cone terminals: an
/// endpoint reached is recorded but not expanded through.  A register's
/// output reflects its input from a previous cycle, not the current one;
/// a boundary port leaves (or enters) the design.
fn walk(
    start: SignalId,
    edges: &IndexMap<SignalId, IndexSet<SignalId>>,
    endpoints: &IndexSet<SignalId>,
) -> IndexSet<SignalId> {
    let mut cone: IndexSet<SignalId> = IndexSet::new();
    let mut visited: IndexSet<SignalId> = IndexSet::new();
    let mut stack: Vec<SignalId> = edges
        .get(&start)
        .map(|set| set.iter().copied().collect())
        .unwrap_or_default();
    while let Some(node) = stack.pop() {
        if endpoints.contains(&node) {
            cone.insert(node);
            continue;
        }
        if !visited.insert(node) {
            continue;
        }
        if let Some(next) = edges.get(&node) {
            stack.extend(next.iter().copied());
        }
    }
    cone
}
