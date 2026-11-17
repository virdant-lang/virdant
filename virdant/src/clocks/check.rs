//! Implements the module-level clock-checking pass of the `Clocking`
//! engine: the submodule clock binding, the skip rules
//! (`@unsafe_clocks`, single-domain fast paths), the declaration-level
//! domain checks (`async` registers, missing `on`/`async` clauses), and
//! the walk that establishes the expression roots handed to the
//! `infer` pass.

use super::*;
use bstr::{BString, ByteSlice};
use crate::analysis::component::{Component, ComponentAnalysis, ComponentId, incoming_clock_ports};
use crate::analysis::dependency::{find_it_context, resolve_it_path_to_owned};
use crate::common::source::Region;
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics;
use crate::syntax::ast::{AstNode, if_arm_children, match_arm_children};
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
        let unsafe_clocks = has_annotation(node, b"unsafe_clocks".as_bstr());
        let clock_count = incoming_clock_ports(self.component_analysis()).len();
        let has_submodules = contains_submodule(node);

        // Submodule clock binding must run before everything else: the
        // checks below (and the walk) see submodule-owned components
        // through the mapped domains it produces.
        if has_submodules {
            self.bind_submodule_clocks(builder, node);
        }

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
        } else {
            if clock_count == 0 {
                // Zero clocks: there is no clock any component could be `Sync`
                // on, so the full coloring has nothing to do — but that only
                // works if every component is explicitly `async` (or
                // Clock-typed: clocks are always `Async`).  A component left
                // unset has no domain at all.
                self.flag_unclad_components(node, "the module has no clocks");
            } else if clock_count >= 2 {
                // Multi-clock module: no unique clock, so nothing can be
                // inferred and every component needs an explicit `on` (or
                // `async`) clause.  (In a unique-clock module the unannotated
                // components keep their implicit `Sync(<unique clock>)`, mixed
                // with `async` or not.)  `reg`/`outgoing reg` (and
                // clock-needing memories) without a clause were already
                // reported as `MissingOnClause` by the component analysis; the
                // remaining kinds are diagnosed here.
                self.flag_unclad_components(node, "the module has multiple clocks");
            }

            // The single-domain fast path skips the walk: every own
            // component is `Sync(<unique clock>)`, so no own join or
            // driver can fail.  A module with submodule instances never
            // takes it: the drivers crossing the module boundary must be
            // checked against the bound parent domains.
            let fast_path =
                clock_count == 1 && !has_async_components(node) && !has_submodules;
            if !fast_path {
                // Color every subexpression of the module and check
                // the module's drivers and connections.
                self.skipped = false;
                self.walk(builder, node, None, &[]);
            }
        }
    }

    /// Binds each submodule instance's incoming clocks, filling
    /// `submodule_domains` with the parent-view domain of every
    /// submodule-owned component of the module.
    ///
    /// Each incoming clock of a submodule — a port, or an inward channel
    /// of one of its sockets — is a free variable in the submodule's
    /// definition: the submodule's own clock analysis phrases its
    /// internal `Sync` domains in those clocks.  Driving the instance's
    /// clock port instantiates the variable: the driving expression, a
    /// plain reference to a Clock-typed component of this module, *is*
    /// the concrete clock, and every child component clocked by the
    /// bound variable takes its domain.  Child components that are
    /// `async` or `Constant` keep their domains; a child clock the
    /// parent never drove leaves the child components clocked by it
    /// unmapped, and so unchecked here.
    fn bind_submodule_clocks(&mut self, builder: &mut Builder, node: &AstNode<'_>) {
        let parsing = node.parsing();
        let mut drivers: Vec<(AstNode<'_>, Option<BString>)> = vec![];
        collect_driver_statements(node, &mut drivers);

        for stmt in node.children() {
            let AstNodePayload::Submodule(module) = stmt.payload() else {
                continue;
            };
            let instance_name = parsing.string(module.name).to_owned();
            let Some(ofness_node) = stmt.ofness() else {
                continue;
            };
            let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                continue;
            };
            // An unresolved submodule name was already reported by the
            // component analysis when it registered the instance's ports.
            let packages = builder.get_packages();
            let submodule_name = parsing.string(ofness.name);
            let qualifier =
                ofness.package.and_then(|pkg| packages.id(parsing.string(pkg)));
            let Some(submodule_symbol) = crate::analysis::package::resolve_item_reference(
                builder,
                submodule_name,
                self.item.package(),
                qualifier,
            ) else {
                continue;
            };
            let child_analysis = builder.get_component_analysis(submodule_symbol.id());

            // The instance's clock-binding sites: drivers whose target is
            // one of the instance's incoming Clock ports.
            let mut bindings: IndexMap<ComponentId, Domain> = IndexMap::new();
            for (driver, context) in &drivers {
                let Some(target) = driver.target() else { continue };
                let target = resolve_it_path_to_owned(
                    parsing.string(target),
                    context.as_ref().map(|ctx| ctx.as_bstr()),
                );
                let Some(target_component) =
                    self.component_analysis().resolve(target.as_bstr())
                else {
                    continue;
                };
                if !is_instance_clock_port(&target_component) {
                    continue;
                }
                let Some(rhs) = driver.driver() else { continue };
                if let Some(domain) = clock_identity(
                    &rhs,
                    parsing,
                    context.as_ref().map(|ctx| ctx.as_bstr()),
                    self.component_analysis(),
                ) {
                    // A port driven more than once keeps its first
                    // binding; conflicting re-drives are driver
                    // analysis's business, not the clock checker's.
                    bindings.entry(target_component.id()).or_insert(domain);
                }
            }

            // Map each child component with a declared domain onto its
            // parent-side instance path; components with no parent-side
            // counterpart (a submodule instance's own internals) drop out
            // when the resolution fails.
            let mut instance_prefix = bstr::BString::from(instance_name);
            instance_prefix.push(b'.');
            for (child_path, child_component) in child_analysis.components() {
                let Some(child_domain) = child_component.domain() else { continue };
                let mut parent_path = instance_prefix.clone();
                parent_path.extend_from_slice(&child_path);
                let Some(parent_component) =
                    self.component_analysis().resolve(parent_path.as_bstr())
                else {
                    continue;
                };
                let mapped = match child_domain {
                    Domain::Async | Domain::Constant => Some(*child_domain),
                    Domain::Sync(child_clock) => child_analysis
                        .component(*child_clock)
                        .map(|clock| clock.path())
                        .and_then(|clock_path| {
                            let mut port_path = instance_prefix.clone();
                            port_path.extend_from_slice(&clock_path);
                            self.component_analysis()
                                .resolve(port_path.as_bstr())
                                .and_then(|port| bindings.get(&port.id()).copied())
                        }),
                };
                if let Some(domain) = mapped {
                    self.submodule_domains.insert(parent_component.id(), domain);
                }
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
    /// arm bodies, `let` bindings, `on` clauses), applies the
    /// `if`/`match` join rules in both expression and statement
    /// position, and checks the module's drivers and connections
    /// against the domains the checker established.
    ///
    /// `context` is the component whose it-block (or memory body)
    /// encodes the walk's current position — the component an `it`
    /// driver target names (`initial` targets are not registered as
    /// references, so their `it` needs this context to resolve).
    /// `guards` stacks the enclosing statement-position `when` guards
    /// and `match` subjects — each as its domain and canonical
    /// [`Location`]: a driver gated by a guard is partly driven by it,
    /// so the guard's domain must also upcast to the driven
    /// component's declared domain.
    fn walk(
        &mut self,
        builder: &mut Builder,
        node: &AstNode<'_>,
        context: Option<ComponentId>,
        guards: &[(Domain, Location)],
    ) {
        match node.payload() {
            AstNodePayload::Component(payload) => {
                if let Some(clause) = node.on_clause() {
                    self.infer(&clause);
                }
                let name = node.parsing().string(payload.name);
                let context = self
                    .component_analysis()
                    .resolve(name.as_bstr())
                    .map(|component| component.id());
                for child in node.children() {
                    self.walk(builder, &child, context, guards);
                }
            }
            AstNodePayload::Socket(payload) => {
                // Drivers inside a socket instance's it-block target
                // its channels (`it.ack := it.req`).
                let name = node.parsing().string(payload.name);
                let context = self
                    .component_analysis()
                    .resolve(name.as_bstr())
                    .map(|component| component.id());
                for child in node.children() {
                    self.walk(builder, &child, context, guards);
                }
            }
            AstNodePayload::Driver(_) | AstNodePayload::InitialDriver => {
                if let Some(rhs) = node.driver() {
                    self.infer(&rhs);
                }
                self.check_driver(builder, node, context, guards);
            }
            AstNodePayload::IndexedDriver(_) => {
                if let Some(index) = node.driver_index() {
                    self.infer(&index);
                }
                if let Some(rhs) = node.driver() {
                    self.infer(&rhs);
                }
                self.check_driver(builder, node, context, guards);
            }
            AstNodePayload::BidirectionalDriver => {
                self.check_connection(node);
            }
            AstNodePayload::OnRegion => {
                // The region's clock is checked against the memory's
                // domain by the memory-write checks (the clock itself
                // is always `Async`, so it must not join here).
                let children = node.children();
                if let Some(clock) = children.first() {
                    self.infer(clock);
                }
                for child in node.children() {
                    self.walk(builder, &child, context, guards);
                }
            }
            AstNodePayload::If => {
                // When the guards conflict among themselves, the `if`
                // is already diagnosed (`DomainMismatch`); its guards
                // then do not constrain the drivers beneath it, or
                // every driver would cascade the same error.
                let joined = self.infer(node);
                let children = node.children();
                for (guard, body) in if_arm_children(&children) {
                    let mut arm_guards = guards.to_vec();
                    if joined.is_some()
                        && let Some(guard) = guard
                        && let Some(domain) = self.infer(guard)
                    {
                        arm_guards.push((domain, guard.location()));
                    }
                    self.walk(builder, &body, context, &arm_guards);
                }
            }
            AstNodePayload::Match => {
                self.infer(node);
                let children = node.children();
                // A statement-position `match` selects the driven
                // value with its subject, so the subject constrains
                // the drivers in its arms exactly like a guard.
                let mut arm_guards = guards.to_vec();
                if let Some(subject) = node.subject()
                    && let Some(domain) = self.infer(&subject)
                {
                    arm_guards.push((domain, subject.location()));
                }
                for (_pat, body) in match_arm_children(&children) {
                    self.walk(builder, &body, context, &arm_guards);
                }
            }
            AstNodePayload::Let => {
                self.infer(node);
                for child in node.children() {
                    self.walk(builder, &child, context, guards);
                }
            }
            _ => {
                for child in node.children() {
                    self.walk(builder, &child, context, guards);
                }
            }
        }
    }

    /// Checks one driver statement against the component it drives:
    /// every contributor to the driven value — the driving expression,
    /// an indexed write's index, and each enclosing `when` guard or
    /// `match` subject — must upcast to the target's declared domain.
    /// Downcasting is never allowed, so in particular a `reg` (whose
    /// declared domain is always `Sync(<its clock>)`) can only be
    /// driven from its own clock's domain (or by constants).
    fn check_driver(
        &mut self,
        builder: &mut Builder,
        node: &AstNode<'_>,
        context: Option<ComponentId>,
        guards: &[(Domain, Location)],
    ) {
        let Some(target) = self.driver_target(node, context) else {
            return;
        };
        // A submodule-owned target takes its bound parent domain; an
        // undetermined domain (unbound instance clock, missing clause)
        // was already diagnosed — do not cascade.
        let Some(target_domain) = self.component_domain(&target) else {
            return;
        };
        let target_name = target.path();

        // The driving expression (and an indexed write's index) is
        // this driver's own; each is checked here.
        let mut contributors: Vec<(Region, Domain)> = vec![];
        if let Some(index) = node.driver_index()
            && let Some(domain) = self.infer(&index)
        {
            contributors.push((index.region(), domain));
        }
        if let Some(rhs) = node.driver()
            && let Some(domain) = self.infer(&rhs)
        {
            contributors.push((rhs.region(), domain));
        }
        for (region, domain) in contributors {
            if !domain.upcasts_to(&target_domain) {
                self.diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::DriverDomainMismatch {
                        target: target_name.clone(),
                        target_domain: self.render_domain(&target_domain),
                        expr_domain: self.render_domain(&domain),
                    },
                ));
            }
        }

        // The enclosing guards are shared by every driver they gate:
        // each (guard, target) pair is judged once, so one bad guard
        // is reported once per component it wrongly gates, not once
        // per driver beneath it.
        for (domain, guard) in guards {
            if !self.guard_checks.insert((*guard, target.id())) {
                continue;
            }
            if !domain.upcasts_to(&target_domain) {
                self.diagnostics.push(Diagnostic::new(
                    builder.get_location_region(*guard),
                    diagnostics::DriverDomainMismatch {
                        target: target_name.clone(),
                        target_domain: self.render_domain(&target_domain),
                        expr_domain: self.render_domain(domain),
                    },
                ));
            }
        }
    }

    /// Resolves the component a driver statement drives.  Regular
    /// driver targets are registered as references by the component
    /// analysis (including `it`-context rewrites); `initial` targets
    /// are not, so their written path is resolved here, rewriting
    /// `it`/`it.` through the enclosing component.
    fn driver_target(
        &self,
        node: &AstNode<'_>,
        context: Option<ComponentId>,
    ) -> Option<Component> {
        let target_node = node.driver_target_node()?;
        if let Some(component) = self
            .component_analysis()
            .component_at(target_node.location())
        {
            return Some(component.clone());
        }
        let path = node.target()?;
        let context_path = context
            .and_then(|context| self.component_analysis().component(context))
            .map(|component| component.path());
        let name = resolve_it_path_to_owned(
            node.parsing().string(path),
            context_path.as_ref().map(|path| path.as_bstr()),
        );
        self.component_analysis().resolve(name.as_bstr())
    }

    /// Checks one `:=:` / `#=#` connection: both operands join to one
    /// domain.  A connection is bidirectional, so neither side is
    /// "expected"; the operands join exactly as the operands of a
    /// combinational expression do — in particular, two distinct
    /// clock domains can never be connected.
    fn check_connection(&mut self, node: &AstNode<'_>) {
        let Some(lhs) = node.connection_lhs() else {
            return;
        };
        let Some(rhs) = node.connection_rhs() else {
            return;
        };
        let lhs_domain = self
            .component_analysis()
            .component_at(lhs.location())
            .and_then(|component| self.component_domain(component));
        let rhs_domain = self
            .component_analysis()
            .component_at(rhs.location())
            .and_then(|component| self.component_domain(component));
        if let (Some(Domain::Sync(lhs_clock)), Some(Domain::Sync(rhs_clock))) =
            (lhs_domain, rhs_domain)
            && lhs_clock != rhs_clock
        {
            self.diagnostics.push(Diagnostic::new(
                node.region(),
                diagnostics::DomainMismatch {
                    left: self.render_domain(&Domain::Sync(lhs_clock)),
                    right: self.render_domain(&Domain::Sync(rhs_clock)),
                },
            ));
        }
    }
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

/// Collects every driver statement of the module (it-blocks, socket
/// it-blocks, and component forwarding blocks alike), each with its
/// `it` context as `find_it_context` resolves it.
fn collect_driver_statements<'p>(
    node: &AstNode<'p>,
    out: &mut Vec<(AstNode<'p>, Option<BString>)>,
) {
    if matches!(
        node.payload(),
        AstNodePayload::Driver(_) | AstNodePayload::InitialDriver | AstNodePayload::IndexedDriver(_)
    ) {
        out.push((node.clone(), find_it_context(node)));
        return;
    }
    for child in node.children() {
        collect_driver_statements(&child, out);
    }
}

/// Whether `component` is one of a submodule instance's incoming clock
/// ports: a submodule-owned incoming component typed `Clock` — an
/// instance's clock port, or the inward Clock channel of one of the
/// instance's sockets.
fn is_instance_clock_port(component: &Component) -> bool {
    component.submodule_owned()
        && component.kind() == Some(ComponentKind::Incoming)
        && component.typ().as_ref() == Some(&Type::Clock)
}

/// The domain the driving expression `rhs` binds an instance's clock
/// port to: a plain reference to a Clock-typed component of this
/// module *is* the concrete clock, keyed by that component.  Anything
/// else leaves the clock unbound (`None`), and the child components
/// clocked by it are not checked in this module.  (A non-clock at a
/// clock port is a type error, reported by the typechecker.)
fn clock_identity(
    rhs: &AstNode<'_>,
    parsing: &Parsing,
    context: Option<&bstr::BStr>,
    component_analysis: &ComponentAnalysis,
) -> Option<Domain> {
    if !matches!(rhs.payload(), AstNodePayload::ExprReference) {
        return None;
    }
    let path = rhs.path()?;
    let name = resolve_it_path_to_owned(parsing.string(path), context);
    let component = component_analysis.resolve(name.as_bstr())?;
    if component.typ().as_ref() != Some(&Type::Clock) {
        return None;
    }
    Some(Domain::sync(component.id()))
}
