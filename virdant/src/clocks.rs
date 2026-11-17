//! Defines the `Clocking` struct, the clock-checking engine for a
//! single module: it assigns a clock [`Domain`] to every component and
//! every subexpression, annotating AST nodes with domains and
//! collecting diagnostics, and orchestrates the `check` and `infer`
//! submodules.

mod check;
mod infer;

use std::sync::Arc;

use bstr::BString;
use indexmap::{IndexMap, IndexSet};

use crate::analysis::component::{Component, ComponentAnalysis, ComponentId};
use crate::analysis::domain::Domain;
use crate::analysis::symbols::Symbol;
use crate::analysis::Location;
use crate::diagnostics::Diagnostic;
use crate::syntax::ast::AstNodeId;

/// The clock-domain analysis of one mod def: the diagnostics it
/// reported and the domain assigned to every checked subexpression.
#[derive(Debug)]
pub struct Clocking {
    item: Symbol,
    component_analysis: Arc<ComponentAnalysis>,
    /// Memoized domain per AST node, `None` for "unknown / not
    /// checked" (unresolvable references, subtrees whose operands
    /// conflicted, or errors reported elsewhere).  Domains are
    /// context-free (they flow bottom-up only), so a shared subtree
    /// (a guard reached both through its `if` and through its own
    /// expr root) is inferred — and diagnosed — exactly once.
    domains: IndexMap<AstNodeId, Option<Domain>>,
    in_progress: IndexSet<AstNodeId>,
    /// Guard domains already checked against a driven component: a
    /// statement-position guard gates every driver in its arms, so
    /// without this memo one bad guard would be reported once per
    /// driver beneath it.  Keyed by (guard, driven component), the
    /// guard named by its canonical [`Location`].
    guard_checks: IndexSet<(Location, ComponentId)>,
    diagnostics: Vec<Diagnostic>,
    /// The parent-view domains of the module's submodule-owned
    /// components, assigned by the submodule clock binding (see
    /// `check::bind_submodule_clocks`).  Components absent from this
    /// map have no parent-view domain and are not checked.
    submodule_domains: IndexMap<ComponentId, Domain>,
    /// Whether checking was skipped for this module (`@unsafe_clocks`,
    /// or one of the single-domain fast paths).
    skipped: bool,
}

impl Clocking {
    pub(crate) fn new(item: Symbol, component_analysis: Arc<ComponentAnalysis>) -> Clocking {
        Clocking {
            item,
            component_analysis,
            domains: IndexMap::new(),
            in_progress: IndexSet::new(),
            guard_checks: IndexSet::new(),
            submodule_domains: IndexMap::new(),
            diagnostics: vec![],
            skipped: true,
        }
    }

    pub fn item(&self) -> &Symbol {
        &self.item
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// The domain assigned to a subexpression, or `None` when the
    /// expression was not checked (skipped module, uninferable
    /// subtree, or an error reported elsewhere).
    pub fn domain_of(&self, node_id: AstNodeId) -> Option<Domain> {
        self.domains.get(&node_id).copied().flatten()
    }

    pub fn skipped(&self) -> bool {
        self.skipped
    }

    /// Records the domain of an AST node.  A node's domain must never
    /// change once recorded: a conflicting re-annotation is a bug in
    /// the checking pass (a redundant consistent one is tolerated).
    pub fn annotate(&mut self, node_id: AstNodeId, domain: Domain) {
        let previous = self.domains.insert(node_id, Some(domain));
        if let Some(Some(previous_domain)) = previous {
            if previous_domain != domain {
                panic!(
                    "Node is already annotated with {previous_domain:?}, can't annotate with {domain:?}"
                );
            }
            // Redundant but consistent annotation: no action needed.
        }
    }

    /// Records that a node has no domain (uninferable or unchecked).
    /// Like [`Clocking::annotate`], this is reached at most once per
    /// node: `infer` consults the memo table first.
    pub(crate) fn annotate_unknown(&mut self, node_id: AstNodeId) {
        self.domains.insert(node_id, None);
    }

    /// Renders a domain for a diagnostic, resolving the clock of a
    /// `Sync` domain to its component path.
    pub(crate) fn render_domain(&self, domain: &Domain) -> BString {
        match domain {
            Domain::Sync(clock) => {
                let path = self
                    .component_analysis
                    .component(*clock)
                    .map(|component| component.path())
                    .unwrap_or_else(|| BString::from("<?clock?>"));
                format!("clocked by `{}`", path).into()
            }
            Domain::Async => BString::from("async"),
            Domain::Constant => BString::from("constant"),
        }
    }

    pub(crate) fn component_analysis(&self) -> &ComponentAnalysis {
        &self.component_analysis
    }

    /// The clock domain of a component in this module's view: the
    /// submodule-bound domain for a submodule-owned component (see
    /// `submodule_domains`), else the component's declared domain.
    pub(crate) fn component_domain(&self, component: &Component) -> Option<Domain> {
        self.submodule_domains
            .get(&component.id())
            .copied()
            .or_else(|| component.domain().copied())
    }
}
