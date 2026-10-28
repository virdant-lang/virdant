//! Immutable, design-derived data for the simulator runtime.
//!
//! `Circuit` is built once by `Circuit::new` from a `Db` and a top
//! `SymbolId`, then handed to `Sim::new`.  After construction nothing
//! inside `Circuit` is mutated; it owns the strong references to the
//! elaborated design and every precomputed dataflow table the runtime
//! relies on.  The `Db` reference is only needed during construction
//! and is not retained afterward.
//!

use std::sync::Arc;

use bstr::{BString, ByteSlice};
use indexmap::{IndexMap, IndexSet};

use crate::analysis::symbols::SymbolId;
use crate::analysis::Location;
use crate::analysis::drivers::Driver;
use crate::analysis::elaboration::{SignalId, Elaboration};
use crate::db::Db;
use crate::sim::expr::{Expr, ExprPayload, Referent, driver_to_expr, initial_to_expr};
use crate::sim::layout::{TypeInterner, TypeLayoutIndex};
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

pub(super) struct Circuit {
    pub(super) elaboration: Arc<Elaboration>,
    pub(super) type_layout_index: TypeLayoutIndex,
    /// Dataflow sensitivity lists, densely indexed by `SignalId::index()`.
    /// `sensitivities[p]` is the insertion-ordered list of components whose
    /// driver reads `p`.  Built from the `deps` map (which is only needed
    /// during construction, so it is not stored here).
    pub(super) sensitivities: Vec<Vec<SignalId>>,
    /// Registers clocked by each clock, densely indexed by
    /// `SignalId::index()`, in insertion order.
    pub(super) clock_sensitivities: Vec<Vec<SignalId>>,
    /// Whether each component is a register, densely indexed by
    /// `SignalId::index()`.
    pub(super) registers: Vec<bool>,
    /// Socket-connection aliases: a Sink-side channel's value is a copy
    /// of its Source-side peer's value (`a :=: b`).  Densely indexed by
    /// `SignalId::index()`.
    pub(super) aliases: Vec<Option<SignalId>>,
    /// Driver expressions, densely indexed by `SignalId::index()`;
    /// `None` for components without a driver expression.
    pub(super) exprs: Vec<Option<Arc<Expr>>>,
    /// Constant expressions from `initial` statements, keyed by register.
    pub(super) initial_exprs: IndexMap<SignalId, Arc<Expr>>,
    /// Pre-resolved referents for every component that has a driver expression.
    /// Keyed by the owning component; each value is the DFS-ordered list of
    /// `(referent, target_id)` pairs that `Sim::build_context` zips with live
    /// values to produce an eval `Context`.  Computed once in `Circuit::new`;
    /// the runtime never re-walks the `Expr` tree to find referents.
    pub(super) resolved_referents: IndexMap<SignalId, Vec<(Referent, SignalId)>>,
}

impl Circuit {
    /// Elaborate `top` against `db` and precompute every dataflow
    /// table the runtime needs.  Walks the elaboration once inside
    /// `build_dependencies` (per-component reference walk to derive `deps`),
    /// then collects `registers` and per-component driver `exprs`.
    pub(super) fn new(db: &Db, top: SymbolId) -> Circuit {
        let elaboration = db.get_elaboration(top);

        let mut deps = build_dependencies(db, &elaboration);
        let mut sensitivities = build_sensitivities(&deps);
        let clock_sensitivities = build_clock_sensitivities(&elaboration);

        // Build the type layouts and the type intern table first: the
        // expression conversion below interns every expression node's
        // type into the same table (so `Value`s can carry `TypeId`s).
        let mut interner = TypeInterner::new();
        let layouts = TypeLayoutIndex::build_layouts(db, &elaboration, &mut interner);

        let mut exprs: IndexMap<SignalId, Arc<Expr>> = IndexMap::new();
        let mut initial_exprs: IndexMap<SignalId, Arc<Expr>> = IndexMap::new();
        let mut registers: IndexSet<SignalId> = IndexSet::new();
        let mut aliases: IndexMap<SignalId, SignalId> = IndexMap::new();
        for elab_component in elaboration.components() {
            let elab_id = elab_component.id();
            let env = elab_component.env();
            if let Some(peer_id) = elab_component.alias() {
                // Socket connection (`:=:`): the Sink-side channel's value
                // is its peer's; handled by the runtime, not an Expr.
                aliases.insert(elab_id, peer_id);
            } else if let Some(driver) = elab_component.driver() {
                match driver {
                    // Bidirectional drivers are resolved into aliases above.
                    crate::analysis::drivers::Driver::Bidirectional(_) => {}
                    _ => {
                        let component_typ = elab_component.typ();
                        exprs.insert(elab_id, driver_to_expr(db, driver, &component_typ, env, &mut interner));
                    }
                }
            }
            if let Some(loc) = elab_component.initial_expr() {
                initial_exprs.insert(elab_id, initial_to_expr(db, loc, env, &mut interner));
            }
            if elab_component.is_reg() {
                registers.insert(elab_id);
            }
        }

        let mut resolved_referents: IndexMap<SignalId, Vec<(Referent, SignalId)>> =
            IndexMap::new();
        for (signal_id, expr) in &exprs {
            let entries = compute_resolved_referents(db, &elaboration, *signal_id, expr);
            resolved_referents.insert(*signal_id, entries);
        }

        let type_layout_index = TypeLayoutIndex::from_parts(layouts, interner);




        // Socket aliases are dataflow edges: the source feeds the sink.
        // These inserts happen before the dense conversion so alias edges
        // are preserved in the sensitivity lists.
        for (sink, source) in &aliases {
            deps.entry(*sink).or_default().insert(*source);
            sensitivities.entry(*source).or_default().insert(*sink);
        }

        // Densify the dataflow tables: index each `Vec` by
        // `SignalId::index()`, preserving insertion order so propagation
        // order is unchanged.
        let n = elaboration.components().len();
        let sensitivities = densify(&sensitivities, &elaboration);
        let clock_sensitivities = densify(&clock_sensitivities, &elaboration);
        let mut dense_registers = vec![false; n];
        for id in &registers {
            dense_registers[id.index()] = true;
        }
        let mut dense_aliases: Vec<Option<SignalId>> = vec![None; n];
        for (sink, source) in &aliases {
            dense_aliases[sink.index()] = Some(*source);
        }
        let mut dense_exprs: Vec<Option<Arc<Expr>>> = vec![None; n];
        for (id, expr) in &exprs {
            dense_exprs[id.index()] = Some(expr.clone());
        }

        Circuit {
            elaboration,
            type_layout_index,
            sensitivities,
            clock_sensitivities,
            registers: dense_registers,
            aliases: dense_aliases,
            exprs: dense_exprs,
            initial_exprs,
            resolved_referents,
        }
    }

    /// Pre-resolved referents for `owner_id`'s driver expression, in
    /// expression-DFS order.  Returns an empty slice for components without
    /// a driver expression.  See `Circuit::resolved_referents`.
    pub(super) fn referents(&self, owner_id: SignalId) -> &[(Referent, SignalId)] {
        self.resolved_referents
            .get(&owner_id)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Number of components whose driver reads `signal`.
    pub(super) fn sensitivity_count(&self, signal: SignalId) -> usize {
        self.sensitivities[signal.index()].len()
    }

    /// The `i`-th component (in insertion order) whose driver reads `signal`.
    /// Caller must ensure `i < sensitivity_count(signal)`.
    pub(super) fn sensitivity_at(&self, signal: SignalId, i: usize) -> SignalId {
        self.sensitivities[signal.index()][i]
    }

    /// Number of registers clocked by `clock`.
    pub(super) fn clock_sensitivity_count(&self, clock: SignalId) -> usize {
        self.clock_sensitivities[clock.index()].len()
    }

    /// The `i`-th register (in insertion order) clocked by `clock`.
    /// Caller must ensure `i < clock_sensitivity_count(clock)`.
    pub(super) fn clock_sensitivity_at(&self, clock: SignalId, i: usize) -> SignalId {
        self.clock_sensitivities[clock.index()][i]
    }
}

/// Densify a signal-keyed table into a `Vec` indexed by `SignalId::index()`,
/// preserving each entry's insertion order.  Components with no entry get an
/// empty list.
fn densify(
    table: &IndexMap<SignalId, IndexSet<SignalId>>,
    elaboration: &Elaboration,
) -> Vec<Vec<SignalId>> {
    elaboration
        .components()
        .iter()
        .map(|c| {
            table
                .get(&c.id())
                .map(|set| set.iter().copied().collect())
                .unwrap_or_default()
        })
        .collect()
}

/// Resolve every `Referent::Component` inside `expr` to the
/// `ElaboratedComponentId` it names, given that `expr` lives on `owner_id`
/// (so references are evaluated in `owner_id`'s scope).
///
/// Returns entries in expression-DFS order, preserving duplicates — the eval
/// `Context` walks them in reverse insertion order, so order matters.
/// `Referent::Location` entries (pattern-bound variables) carry no
/// `ElaboratedComponentId` and are dropped here; they are bound at runtime
/// inside `eval_match` instead.
fn compute_resolved_referents(
    db: &Db,
    elaboration: &Elaboration,
    owner_id: SignalId,
    expr: &Expr,
) -> Vec<(Referent, SignalId)> {
    let ec = elaboration.component(owner_id);
    let prefix = ec.driver_scope().to_str_lossy().into_owned();

    let mut out = Vec::new();
    for referent in collect_referents(expr) {
        if let Referent::Owner = &referent {
            out.push((referent, owner_id));
            continue;
        }
        if let Referent::Component(comp_id) = &referent {
            let comp_analysis = db.get_component_analysis(comp_id.item_id());
            if let Some(comp) = comp_analysis.component(*comp_id) {
                let full_path: BString = format!("{prefix}.{}", comp.path()).into();
                if let Some(elab_comp) = elaboration.resolve(&full_path) {
                    out.push((referent, elab_comp.id()));
                } else {
                    eprintln!("DEBUG unresolved referent: owner={:?} prefix={prefix:?} comp_path={:?} full={full_path:?}", elaboration.component(owner_id).path().as_bstr(), comp.path().as_bstr());
                }
            }
        }
    }
    out
}

/// Build the data-flow dependency map for all elaborated components.
///
/// A component A depends on component B if B's path appears as a reference
/// inside A's driver expression(s).  References in drivers are relative to the
/// owning module's scope; this function converts them to absolute elaborated
/// paths before resolving.
fn build_dependencies(
    db: &Db,
    elaboration: &Elaboration,
) -> IndexMap<SignalId, IndexSet<SignalId>> {
    let mut dep_map: IndexMap<SignalId, IndexSet<SignalId>> =
        IndexMap::new();

    for component in elaboration.components() {
        let mut deps: IndexSet<SignalId> = IndexSet::new();

        if let Some(driver) = component.driver() {
            let prefix = component.driver_scope().to_str_lossy().into_owned();
            for loc in collect_locations(driver) {
                let parsing = db.get_parsing(loc.package());
                for name in walk_expr_refs(&parsing, loc.ast_node_id()) {
                    let full_path: BString = format!("{prefix}.{name}").into();
                    if let Some(dep) = elaboration.resolve(&full_path) {
                        deps.insert(dep.id());
                    }
                    // If resolve returns None the name is a pattern-bound
                    // variable (e.g. from a match arm) or otherwise not a
                    // component; either way it contributes no dependency.
                }
            }
        }

        dep_map.insert(component.id(), deps);
    }

    dep_map
}

/// Invert the dependency map into a sensitivity map.
///
/// `deps[a]` = set of components that `a` reads from.
/// `sensitivities[p]` = set of components that read from `p`.
///
/// Formally: `p ∈ sensitivities[a]  ⟺  a ∈ deps[p]`.
fn build_sensitivities(
    deps: &IndexMap<SignalId, IndexSet<SignalId>>,
) -> IndexMap<SignalId, IndexSet<SignalId>> {
    let mut sens: IndexMap<SignalId, IndexSet<SignalId>> =
        IndexMap::new();
    for (&dependent, dep_set) in deps {
        for &dependency in dep_set {
            sens.entry(dependency).or_default().insert(dependent);
        }
    }
    sens
}

/// Map each clock component to the set of registers that list it in their `on` clause.
///
/// This reads directly from `ElaboratedComponent::clock()`, which is already
/// resolved to an `ElaboratedComponentId` during elaboration.
fn build_clock_sensitivities(
    elaboration: &Elaboration,
) -> IndexMap<SignalId, IndexSet<SignalId>> {
    let mut map: IndexMap<SignalId, IndexSet<SignalId>> =
        IndexMap::new();
    for component in elaboration.components() {
        if let Some(clock_id) = component.clock() {
            map.entry(clock_id).or_default().insert(component.id());
        }
    }
    map
}

/// Recursively collect every expression `Location` from a `Driver`.
///
/// For `Driver::Expr` this is the single expression location.
/// For `Driver::If` this includes every condition location plus all
/// locations from every sub-driver and the else clause.
fn collect_locations(driver: &Driver) -> Vec<Location> {
    let mut locs = Vec::new();
    collect_locations_inner(driver, &mut locs);
    locs
}

fn collect_locations_inner(driver: &Driver, locs: &mut Vec<Location>) {
    match driver {
        Driver::Expr(_, loc) => locs.push(loc.clone()),
        Driver::Bidirectional(_) => {}
        Driver::Absent => {}
        Driver::When(driver_when) => {
            for (cond_loc, sub_driver) in &driver_when.clauses {
                locs.push(cond_loc.clone());
                collect_locations_inner(sub_driver, locs);
            }
            if let Some(else_driver) = &driver_when.else_clause {
                collect_locations_inner(else_driver, locs);
            }
        }
        Driver::Match(driver_match) => {
            locs.push(driver_match.subject.clone());
            for (pat_loc, sub_driver) in &driver_match.arms {
                locs.push(pat_loc.clone());
                collect_locations_inner(sub_driver, locs);
            }
            if let Some(else_driver) = &driver_match.else_clause {
                collect_locations_inner(else_driver, locs);
            }
        }
    }
}

/// DFS over the AST subtree rooted at `root_id`, collecting the name string
/// of every `ExprReference` node encountered.  Each name is returned with
/// any enclosing `it`-block prefix already substituted for the binding name
/// of its surrounding `Submodule` / `Component` / `Socket`, matching the
/// rewriting performed by `sim::expr::convert_ast_expr`.
fn walk_expr_refs(parsing: &Parsing, root_id: AstNodeId) -> Vec<BString> {
    let mut result = Vec::new();
    let mut stack = vec![root_id];
    while let Some(id) = stack.pop() {
        let node = parsing.ast_node(id);
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(interned) = node.path() {
                let raw = parsing.string(interned);
                result.push(rewrite_it_in_name(parsing, id, raw));
            }
        }
        for child in node.children() {
            stack.push(child.id());
        }
    }
    result
}

/// Rewrite a path of the form `it` or `it.<rest>` to use the binding name
/// of the nearest enclosing `Submodule` / `Component` / `Socket`.  Returns
/// the path unchanged when it does not start with `it`.
fn rewrite_it_in_name(parsing: &Parsing, ast_id: AstNodeId, raw: &bstr::BStr) -> BString {
    if !raw.starts_with(b"it.") && raw != b"it" {
        return raw.to_owned();
    }
    let mut current_id = ast_id;
    loop {
        let current = parsing.ast_node(current_id);
        if current.is_item() {
            return raw.to_owned();
        }
        let target: Option<BString> = match current.payload() {
            AstNodePayload::Submodule(m) => Some(parsing.string(m.name).to_owned()),
            AstNodePayload::Component(c) => Some(parsing.string(c.name).to_owned()),
            AstNodePayload::Socket(s) => Some(parsing.string(s.name).to_owned()),
            _ => None,
        };
        if let Some(target) = target {
            if raw == b"it" { return target; }
            let mut out = target;
            out.push(b'.');
            out.extend_from_slice(&raw[3..]);
            return out;
        }
        current_id = match current.parent() {
            Some(p) => p.id(),
            None => return raw.to_owned(),
        };
    }
}

/// DFS-walk an `Expr` tree and collect every `Referent` found in a
/// `Reference` node.  Duplicate referents are included as-is; the `Context`
/// lookup handles them by searching in reverse insertion order.
fn collect_referents(expr: &Expr) -> Vec<Referent> {
    let mut result = Vec::new();
    collect_referents_inner(expr, &mut result);
    result
}

fn collect_referents_inner(expr: &Expr, out: &mut Vec<Referent>) {
    match expr.payload() {
        ExprPayload::Reference(r)       => out.push(r.referent.clone()),
        ExprPayload::Paren(p)           => collect_referents_inner(&p.subject, out),
        ExprPayload::When(w)            => {
            for (cond, body) in &w.branches {
                collect_referents_inner(cond, out);
                collect_referents_inner(body, out);
            }
            collect_referents_inner(&w.else_branch, out);
        }
        ExprPayload::Match(m)           => {
            collect_referents_inner(&m.subject, out);
            for (_, body) in &m.arms { collect_referents_inner(body, out); }
        }
        ExprPayload::BinOp(b)          => {
            collect_referents_inner(&b.lhs, out);
            collect_referents_inner(&b.rhs, out);
        }
        ExprPayload::UnOp(u)           => collect_referents_inner(&u.subject, out),
        ExprPayload::Method(m)         => {
            collect_referents_inner(&m.subject, out);
            for arg in &m.args { collect_referents_inner(arg, out); }
        }
        ExprPayload::Fn(f)             => {
            collect_referents_inner(&f.subject, out);
            for arg in &f.args { collect_referents_inner(arg, out); }
        }
        ExprPayload::Ctor(c)           => {
            for arg in &c.args { collect_referents_inner(arg, out); }
        }
        ExprPayload::Struct(s)         => {
            for (_, field) in &s.fields { collect_referents_inner(field, out); }
        }
        ExprPayload::Index(i)          => collect_referents_inner(&i.subject, out),
        ExprPayload::IndexDyn(i)       => {
            collect_referents_inner(&i.subject, out);
            collect_referents_inner(&i.index, out);
        }
        ExprPayload::IndexRange(i)     => collect_referents_inner(&i.subject, out),
        ExprPayload::Word(w)           => {
            for arg in &w.args { collect_referents_inner(arg, out); }
        }
        ExprPayload::Zext(z)           => collect_referents_inner(&z.subject, out),
        ExprPayload::Sext(s)           => collect_referents_inner(&s.subject, out),
        ExprPayload::AsClock(a)        => collect_referents_inner(&a.subject, out),
        ExprPayload::Cast(c)           => collect_referents_inner(&c.subject, out),
        ExprPayload::Trunc(t)          => collect_referents_inner(&t.subject, out),
        ExprPayload::Any(a)            => collect_referents_inner(&a.subject, out),
        ExprPayload::All(a)            => collect_referents_inner(&a.subject, out),
        ExprPayload::As(a)             => collect_referents_inner(&a.subject, out),
        // Leaves — no sub-expressions to recurse into.
        ExprPayload::BitLit(_)
        | ExprPayload::WordLit(_)
        | ExprPayload::StrLit(_)
        | ExprPayload::Enumerant(_)
        | ExprPayload::Hole(_)
        | ExprPayload::Dontcare(_)     => {}
    }
}
