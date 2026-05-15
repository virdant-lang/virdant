//! Immutable, design-derived data for the simulator runtime.
//!
//! `Circuit` is built once by `Circuit::new` from a `Db` and a top
//! `SymbolId`, then handed to `Sim::new`.  After construction nothing
//! inside `Circuit` is mutated; it owns the strong references to the
//! database, the elaborated design, and every precomputed dataflow
//! table the runtime relies on.
//!

use std::sync::Arc;

use bstr::{BString, ByteSlice};
use indexmap::{IndexMap, IndexSet};

use crate::analysis::symbols::SymbolId;
use crate::analysis::Location;
use crate::analysis::drivers::Driver;
use crate::analysis::elaboration::{ElaboratedComponent, ElaboratedComponentId, Elaboration};
use crate::common::ComponentKind;
use crate::db::Db;
use crate::sim::expr::{Expr, ExprPayload, Referent, driver_to_expr};
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

pub(super) struct Circuit {
    pub(super) db: Arc<Db>,
    pub(super) elaboration: Arc<Elaboration>,
    pub(super) component_paths: IndexMap<ElaboratedComponentId, BString>,
    pub(super) deps: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>>,
    pub(super) sensitivities: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>>,
    pub(super) clock_sensitivities: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>>,
    pub(super) registers: IndexSet<ElaboratedComponentId>,
    pub(super) exprs: IndexMap<ElaboratedComponentId, Arc<Expr>>,
}

impl Circuit {
    /// Elaborate `top` against `db` and precompute every dataflow
    /// table the runtime needs.  Walks the elaboration twice — once
    /// inside `build_dependencies` (per-component reference walk to
    /// derive `deps`), once here to collect `component_paths`,
    /// `registers`, and the per-component driver `exprs`.
    pub(super) fn new(db: Arc<Db>, top: SymbolId) -> Circuit {
        let elaboration = db.get_elaboration(top);

        let deps = build_dependencies(&db, &elaboration);
        let sensitivities = build_sensitivities(&deps);
        let clock_sensitivities = build_clock_sensitivities(&elaboration);

        let mut exprs: IndexMap<ElaboratedComponentId, Arc<Expr>> = IndexMap::new();
        let mut component_paths: IndexMap<ElaboratedComponentId, BString> = IndexMap::new();
        let mut registers: IndexSet<ElaboratedComponentId> = IndexSet::new();
        for elab_component in elaboration.components() {
            let elab_id = elab_component.id();
            component_paths.insert(elab_id, elab_component.path().clone());
            if let Some(driver) = elab_component.driver() {
                exprs.insert(elab_id, driver_to_expr(&db, driver));
            }
            if elab_component.is_reg() {
                registers.insert(elab_id);
            }
        }

        Circuit {
            db,
            elaboration,
            component_paths,
            deps,
            sensitivities,
            clock_sensitivities,
            registers,
            exprs,
        }
    }

    /// Resolve every `Referent::Component` inside `expr` to the
    /// `ElaboratedComponentId` it points at, given that `expr` lives on
    /// `owner_id` (so references are evaluated in `owner_id`'s scope).
    ///
    /// Returns the resolved entries in expression-DFS order, preserving any
    /// duplicate referents \u2014 the eval `Context` walks them in reverse
    /// insertion order, so order matters.  `Referent::Location` entries
    /// (pattern-bound variables) carry no `ElaboratedComponentId` and are
    /// dropped here; callers wanting the live `Value` for them must look
    /// them up by `Location` separately.
    pub(super) fn resolve_expr_referents(
        &self,
        owner_id: ElaboratedComponentId,
        expr: &Expr,
    ) -> Vec<(Referent, ElaboratedComponentId)> {
        let ec = self.elaboration.component(owner_id);
        let prefix = scope_prefix(ec);

        let mut out = Vec::new();
        for referent in collect_referents(expr) {
            if let Referent::Component(comp_id) = &referent {
                let comp_analysis = self.db.get_component_analysis(comp_id.item_id());
                if let Some(comp) = comp_analysis.component(*comp_id) {
                    let full_path: BString = format!("{prefix}.{}", comp.path()).into();
                    if let Some(elab_comp) = self.elaboration.resolve(&full_path) {
                        out.push((referent, elab_comp.id()));
                    }
                }
            }
        }
        out
    }
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
) -> IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>> {
    let mut dep_map: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>> =
        IndexMap::new();

    for component in elaboration.components() {
        let mut deps: IndexSet<ElaboratedComponentId> = IndexSet::new();

        if let Some(driver) = component.driver() {
            let prefix = scope_prefix(component);
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
    deps: &IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>>,
) -> IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>> {
    let mut sens: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>> =
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
) -> IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>> {
    let mut map: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>> =
        IndexMap::new();
    for component in elaboration.components() {
        if let Some(clock_id) = component.clock() {
            map.entry(clock_id).or_default().insert(component.id());
        }
    }
    map
}

/// Compute the module-scope prefix used to resolve relative references inside
/// a component's driver expression.
///
/// Incoming ports are driven from the *parent* module's scope (one level above
/// the submodule instance), so two path segments are dropped.
/// All other components are driven within their own module, so one is dropped.
fn scope_prefix(component: &ElaboratedComponent) -> String {
    let path = component.path();
    let last_dot = path.rfind_byte(b'.').expect("elaborated path always has at least one dot");
    if component.component_kind() == ComponentKind::Incoming {
        // e.g. "top.gcd.clock" -> "top"
        let second_last = path[..last_dot].rfind_byte(b'.').unwrap_or(0);
        path[..second_last].to_str_lossy().into_owned()
    } else {
        // e.g. "top.gcd.state" -> "top.gcd"
        path[..last_dot].to_str_lossy().into_owned()
    }
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
        Driver::If(driver_if) => {
            for (cond_loc, sub_driver) in &driver_if.clauses {
                locs.push(cond_loc.clone());
                collect_locations_inner(sub_driver, locs);
            }
            if let Some(else_driver) = &driver_if.else_clause {
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
        ExprPayload::If(i)              => {
            for (cond, body) in &i.branches {
                collect_referents_inner(cond, out);
                collect_referents_inner(body, out);
            }
            collect_referents_inner(&i.else_branch, out);
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
        ExprPayload::IndexRange(i)     => collect_referents_inner(&i.subject, out),
        ExprPayload::Word(w)           => {
            for arg in &w.args { collect_referents_inner(arg, out); }
        }
        ExprPayload::Zext(z)           => collect_referents_inner(&z.subject, out),
        ExprPayload::Sext(s)           => collect_referents_inner(&s.subject, out),
        ExprPayload::Cast(c)           => collect_referents_inner(&c.subject, out),
        ExprPayload::Any(a)            => collect_referents_inner(&a.subject, out),
        ExprPayload::All(a)            => collect_referents_inner(&a.subject, out),
        ExprPayload::As(a)             => collect_referents_inner(&a.subject, out),
        // Leaves — no sub-expressions to recurse into.
        ExprPayload::BitLit(_)
        | ExprPayload::WordLit(_)
        | ExprPayload::StrLit(_)
        | ExprPayload::Enumerant(_)
        | ExprPayload::Hole(_)         => {}
    }
}
