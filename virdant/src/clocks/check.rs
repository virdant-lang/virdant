//! Implements the module-level clock-checking pass of the `Clocking`
//! engine: the skip rules (submodules, `@unsafe_clocks`, single-domain
//! fast paths), the declaration-level domain checks (`async` registers,
//! missing `on`/`async` clauses), and the walk that establishes the
//! expression roots handed to the `infer` pass.

use super::*;
use bstr::ByteSlice;
use crate::analysis::component::incoming_clock_ports;
use crate::common::source::Region;
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics::{self, DiagnosticPayload};
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::Parsing;
use crate::types::Type;

impl Clocking {
    /// Clockchecks the mod def this `Clocking` was built for: assigns
    /// a clock [`Domain`] to every component and every subexpression
    /// and verifies the domain rules.  The skip rules and
    /// declaration-level checks live in `check_moddef` below; the
    /// per-expression domain rules live in the `infer` submodule.
    pub(crate) fn check(&mut self, builder: &mut Builder) {
        let parsing = builder.get_parsing(self.item.package());
        let node = parsing.ast_node(builder.get_symbol_ast(self.item.id()));

        if let AstNodePayload::ModDef(mod_def) = node.payload()
            && !mod_def.is_ext
        {
            self.check_moddef(builder, &node);
        }
    }

    /// The module-level checks of one mod def.
    fn check_moddef(&mut self, builder: &mut Builder, node: &AstNode<'_>) {
        // Modules with submodule instances: submodule clock binding is
        // not yet implemented, so the checker cannot color these
        // modules.  Skip entirely, with a TODO note.  The note is
        // suppressed for the standard library (the `.vir` files shipped
        // with the compiler): flagging library modules would put the
        // note into every project's diagnostics.
        if contains_submodule(node) {
            let packages = builder.get_packages();
            if !is_stdlib_package(packages.name(self.item.package())) {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    DiagnosticPayload::Todo(diagnostics::Todo {
                        message: "clockcheck is not yet implemented for modules with submodules; skipping"
                            .into(),
                    }),
                ));
            }
        } else {
            let unsafe_clocks = has_annotation(node, b"unsafe_clocks".as_bstr());
            let clock_count = incoming_clock_ports(self.component_analysis()).len();

            // A register (or memory) can never be `async`: clocked components
            // always live on a `Sync` domain, whatever the module's clocks.
            self.check_async_registers(node);

            if unsafe_clocks {
                // Clockchecking is disabled, but the safety net is the
                // annotations themselves: every declared component must carry
                // an explicit `on` or `async` clause.
                self.flag_unclad_components(
                    node,
                    "the module is marked `@unsafe_clocks`",
                );
            } else if clock_count == 0 {
                // Zero clocks: there is no clock any component could be `Sync`
                // on, so the full coloring has nothing to do — but that only
                // works if every component is explicitly `async` (or
                // Clock-typed: clocks are always `Async`).  A component left
                // unset has no domain at all.
                self.flag_unclad_components(node, "the module has no clocks");
            } else if clock_count == 1 && !has_async_components(node) {
                // Unique clock, no `async` components: the component analysis
                // has already fixed every component's domain to
                // `Sync(<unique clock>)`; there is nothing to mix.
            } else {
                // Multi-clock module: no unique clock, so nothing can be
                // inferred and every component needs an explicit `on` (or
                // `async`) clause.  (In a unique-clock module the unannotated
                // components keep their implicit `Sync(<unique clock>)`, mixed
                // with `async` or not.)  `reg`/`outgoing reg` (and
                // clock-needing memories) without a clause were already
                // reported as `MissingOnClause` by the component analysis; the
                // remaining kinds are diagnosed here.
                if clock_count >= 2 {
                    self.flag_unclad_components(node, "the module has multiple clocks");
                }

                // Color every subexpression of the module.
                self.skipped = false;
                self.walk(node);
            }
        }
    }

    /// Flags every top-level clocked component (`reg`, `outgoing reg`,
    /// `memory`) declared `async`, which is never legal.
    fn check_async_registers(&mut self, node: &AstNode<'_>) {
        let parsing = node.parsing();
        for component in top_level_components(node) {
            let AstNodePayload::Component(payload) = component.payload() else {
                unreachable!()
            };
            if payload.is_async
                && matches!(
                    payload.kind,
                    ComponentKind::Reg | ComponentKind::OutgoingReg | ComponentKind::Memory
                )
            {
                let name = parsing.string(payload.name).to_owned();
                self.diagnostics.push(Diagnostic::new(
                    component.region(),
                    diagnostics::AsyncRegister { component: name },
                ));
            }
        }
    }

    /// Flags every declared component that carries no domain clause at
    /// all (see `unclad_components`), with `reason` explaining why the
    /// module requires explicit clauses.
    fn flag_unclad_components<S: Into<BString> + Clone>(&mut self, node: &AstNode<'_>, reason: S) {
        for (name, region) in
            unclad_components(node, node.parsing(), self.component_analysis())
        {
            self.diagnostics.push(Diagnostic::new(
                region,
                diagnostics::MissingDomainClause {
                    component: name,
                    reason: reason.clone().into(),
                },
            ));
        }
    }

    /// Visits every expression-bearing node of the module.  Domains of
    /// plain subexpressions are filled in by the recursion inside
    /// `infer`; this walk establishes the roots (driver RHSes, guards,
    /// arm bodies, `let` bindings, `on` clauses) and applies the
    /// `if`/`match` join rules in both expression and statement
    /// position.
    fn walk(&mut self, node: &AstNode<'_>) {
        match node.payload() {
            AstNodePayload::Component(_) => {
                if let Some(clause) = node.on_clause() {
                    self.infer(&clause);
                }
            }
            AstNodePayload::Driver(_) | AstNodePayload::InitialDriver => {
                if let Some(rhs) = node.driver() {
                    self.infer(&rhs);
                }
            }
            AstNodePayload::IndexedDriver(_) => {
                if let Some(index) = node.driver_index() {
                    self.infer(&index);
                }
                if let Some(rhs) = node.driver() {
                    self.infer(&rhs);
                }
            }
            AstNodePayload::OnRegion => {
                let children = node.children();
                if let Some(clock) = children.first() {
                    self.infer(clock);
                }
            }
            AstNodePayload::If | AstNodePayload::Match | AstNodePayload::Let => {
                self.infer(node);
            }
            _ => {}
        }
        for child in node.children() {
            self.walk(&child);
        }
    }
}

/// The `.vir` packages shipped with the compiler (everything in
/// `lib/`).  Compiler-owned code is not the user's to fix, so
/// user-facing notes (the submodule skip TODO) are suppressed there.
const STDLIB_PACKAGES: &[&[u8]] =
    &[b"builtin", b"ice40", b"virt", b"xilinx"];

fn is_stdlib_package(name: &bstr::BStr) -> bool {
    STDLIB_PACKAGES.contains(&name.as_bytes())
}

/// The `Component` statements at the top level of a mod def.
fn top_level_components<'p, 'a>(moddef: &AstNode<'p>) -> Vec<AstNode<'p>> {
    moddef
        .children()
        .into_iter()
        .filter(|child| matches!(child.payload(), AstNodePayload::Component(_)))
        .collect()
}

/// Whether any top-level component of the mod def is declared `async`.
fn has_async_components(moddef: &AstNode<'_>) -> bool {
    top_level_components(moddef).into_iter().any(|component| {
        matches!(
            component.payload(),
            AstNodePayload::Component(payload) if payload.is_async
        )
    })
}

/// The declared components of the module that carry no domain clause
/// at all: not `async`, no `on` clause, not Clock-typed (clocks are
/// always `Async` and carry no clause), and not
/// `reg`/`outgoing reg`/`memory` (those can never be `async` at all; a
/// missing clock there is `MissingOnClause`'s business, already
/// reported by the component analysis).  Callers decide what the
/// missing clause means (no clocks, multiple clocks, or
/// `@unsafe_clocks`).
fn unclad_components(
    moddef: &AstNode<'_>,
    parsing: &Parsing,
    component_analysis: &ComponentAnalysis,
) -> Vec<(bstr::BString, Region)> {
    top_level_components(moddef)
        .into_iter()
        .filter_map(|component| {
            let AstNodePayload::Component(payload) = component.payload() else {
                unreachable!()
            };
            if payload.is_async || component.on_clause().is_some() {
                return None;
            }
            if matches!(
                payload.kind,
                ComponentKind::Reg
                    | ComponentKind::OutgoingReg
                    | ComponentKind::Memory
            ) {
                return None;
            }
            let name = parsing.string(payload.name).to_owned();
            // The declared domain is no guide here: unique-clock
            // inference has already filled every unset domain in a
            // unique-clock module.  The annotation itself is the test.
            if component_analysis
                .resolve(name.as_bstr())
                .and_then(|component| component.typ())
                .is_some_and(|typ| typ == Type::Clock)
            {
                return None;
            }
            Some((name, component.region()))
        })
        .collect()
}

/// Whether any statement of the mod def (recursively) instantiates a
/// submodule (`mod foo of Bar`).
fn contains_submodule(node: &AstNode<'_>) -> bool {
    if matches!(node.payload(), AstNodePayload::Submodule(_)) {
        return true;
    }
    node.children().into_iter().any(|child| contains_submodule(&child))
}

/// Whether the mod def carries an `@name` annotation.
fn has_annotation(node: &AstNode<'_>, name: &bstr::BStr) -> bool {
    let parsing = node.parsing();
    node.annotations().into_iter().any(|annotation| {
        annotation
            .annotation_name()
            .map(|annotation_name| parsing.string(annotation_name) == name)
            .unwrap_or(false)
    })
}
