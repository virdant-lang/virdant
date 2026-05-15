use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::sync::Arc;
use std::time::Duration;

use bstr::{BStr, BString, ByteSlice};
use indexmap::{IndexMap, IndexSet};

use crate::analysis::symbols::SymbolId;
use crate::analysis::Location;
use crate::analysis::drivers::Driver;
use crate::analysis::elaboration::{ElaboratedComponent, ElaboratedComponentId, Elaboration};
use crate::common::ComponentKind;
use crate::db::Db;
use crate::sim::eval::{Context, Value};
use crate::sim::expr::{Expr, ExprPayload, Referent, driver_to_expr};
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;


#[derive(Eq, Hash, PartialEq, Debug)]
pub struct Node {
    component_id: ElaboratedComponentId,
    is_reg_set: bool,
}

#[derive(Debug)]
pub struct Sim {
    db: Arc<Db>,
    values: IndexMap<Node, Value>,
    component_paths: IndexMap<ElaboratedComponentId, BString>,
    top: SymbolId,
    deps: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>>,
    sensitivities: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>>,
    clock_sensitivities: IndexMap<ElaboratedComponentId, IndexSet<ElaboratedComponentId>>,
    dirty: IndexSet<ElaboratedComponentId>,
    registers: IndexSet<ElaboratedComponentId>,
    exprs: IndexMap<ElaboratedComponentId, Arc<Expr>>,

    sim_time_ps: u64,
    shutdown_requested: bool,
    queue: BinaryHeap<Reverse<ScheduledEvent>>,
    next_seq: u64,
    // TODO(callback-api): Callback / CallbackFn live below as dead code
    // until iteration 2, when we design the real callback API against
    // ergonomics feedback from playing with iteration 1's print-only
    // event loop. See SIM_DESIGN.md (Q3 / D5).
    #[allow(dead_code)]
    callbacks: Vec<Callback>,
}


#[derive(Debug, Eq, PartialEq)]
pub enum Event {
//    ValueChange(ElaboratedComponentId), // Signal value changed
//    Stmt, // Statement execution
//    Force, // Force operation on signal
//    Release, // Release operation on signal
//    AtStartOfSimTime, // At start of simulation time
//    ReadWriteSync, // Read/write synchronization
//    ReadOnlySync, // Read-only synchronization
//    NextSimTime, // Next simulation time
//    AfterDelayInPs(u64), // After specified delay
//    EndOfCompile, // End of compilation
    StartOfSimulation, // Start of simulation
    EndOfSimulation, // End of simulation
//    Error, // Simulator error
//    TchkViolation, // Timing check violation
//    StartOfSave, // Start of save operation
//    EndOfSave, // End of save operation
//    StartOfRestart, // Start of restart
//    EndOfRestart, // End of restart
//    StartOfReset, // Start of reset
//    EndOfReset, // End of reset
//    EnterInteractive, // Enter interactive mode
//    ExitInteractive, // Exit interactive mode
//    InteractiveScopeChange, // Interactive scope changed
//    UnresolvedSystf, // Unresolved system function
}

/// One scheduled event in the time queue.  Ordered by ascending `at_ps`,
/// then ascending `seq` so same-time events fire in registration (FIFO)
/// order.  `seq` is globally unique per `Sim`, so equality reduces to
/// equality of `seq`.
#[derive(Debug, Eq, PartialEq)]
struct ScheduledEvent {
    at_ps: u64,
    seq:   u64,
    event: Event,
}

impl Ord for ScheduledEvent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.at_ps.cmp(&other.at_ps).then(self.seq.cmp(&other.seq))
    }
}
impl PartialOrd for ScheduledEvent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(other)) }
}

#[derive(Debug)]
pub enum SimError {}

pub enum CallbackFn {
    Safe(Box<dyn Fn()>),
//    Unsafe(
}

impl std::fmt::Debug for CallbackFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(CallbackFn)")
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Callback {
    pub event:    Event,
    pub cb_rtn:    Option<CallbackFn>,
//    pub user_data: *mut PliByte8,
// True once `cb_rtn` has been invoked at least once.  `cbValueChange` is
// persistent (we clear this on every fire); other reasons are one-shot.
//    pub fired:     bool,
// True once `vpi_remove_cb` has been called; the entry stays in the
// table but is skipped when firing / draining a queue.
//    pub removed:   bool,
// Absolute simulation time at which a `cbAfterDelay` should fire.
// Meaningful only when `reason == cbAfterDelay`.
//    pub deadline_ps: u64,
}

impl Sim {
    pub fn new(db: Arc<Db>, top: SymbolId) -> Sim {
        let elaboration = db.get_elaboration(top);

        let deps = build_dependencies(&db, &elaboration);
        let sensitivities = build_sensitivities(&deps);
        let clock_sensitivities = build_clock_sensitivities(&elaboration);

        let mut exprs: IndexMap<ElaboratedComponentId, Arc<Expr>> = IndexMap::new();
        for elab_component in elaboration.components() {
            if let Some(driver) = elab_component.driver() {
                exprs.insert(elab_component.id(), driver_to_expr(&db, driver));
            }
        }

        let mut sim = Sim {
            values: IndexMap::new(),
            component_paths: IndexMap::new(),
            db: db.clone(),
            top,
            deps,
            sensitivities,
            clock_sensitivities,
            dirty: IndexSet::new(),
            registers: IndexSet::new(),
            exprs,
            sim_time_ps: 0,
            shutdown_requested: false,
            queue: BinaryHeap::new(),
            next_seq: 0,
            callbacks: vec![],
        };

        for elab_component in elaboration.components() {
            let elab_component_id = elab_component.id();
            let typ = elab_component.typ();

            sim.component_paths.insert(elab_component_id, elab_component.path().clone());

            let node = Node::val(elab_component_id);
            let value = Value::X(typ.clone());
            sim.values.insert(node, value);

            if elab_component.is_reg() {
                let node = Node::set(elab_component_id);
                let value = Value::X(typ);
                sim.values.insert(node, value);

                sim.registers.insert(elab_component_id);
            }
        }

        // Evaluate all components whose expression has no dependencies (empty deps set).
        // These are constant-valued nodes — their driver reads nothing from the simulation
        // state, so their value is fixed and can be determined immediately.  Collect the
        // IDs first to satisfy the borrow checker, then evaluate and set each one.
        let constant_ids: Vec<ElaboratedComponentId> = sim
            .deps
            .iter()
            .filter(|(id, dep_set)| dep_set.is_empty() && sim.exprs.contains_key(*id))
            .map(|(id, _)| *id)
            .collect();

        for id in constant_ids {
            let expr = sim.exprs[&id].clone();
            let context = sim.build_context(id, &expr);
            let value = expr.eval(context);
            sim.set(id, value);
        }

        // Propagate constant values to all transitively-dependent components.
        sim.flow();

        sim
    }

    fn elaboration(&self) -> Arc<Elaboration> {
        self.db.get_elaboration(self.top)
    }

    pub fn dump(&self) {
        let elaboration = self.elaboration();

        for (node, value) in &self.values {
            let path = &self.component_paths.get(&node.component_id).unwrap();
            if node.is_reg_set {
                let clock_id = elaboration.component(node.component_id).clock().unwrap();
                let clock = elaboration.component(clock_id);
                println!("{path} <= {value:?} on {:?}", clock.path());
            } else {
                println!("{path} := {value:?}");
            }
        }
    }

    pub fn tick(&mut self, component_id: ElaboratedComponentId) {
        self.tick_prop(component_id);
        self.flow();
    }

    fn tick_prop(&mut self, component_id: ElaboratedComponentId) {
        if let Some(sensitivities) = self.sensitivities.get(&component_id) {
            for component_id in sensitivities.clone() {
                self.tick(component_id);
            }
        }

        if let Some(clock_sensitivities) = self.clock_sensitivities.get(&component_id) {
            let clock_sensitivities = clock_sensitivities.clone();
            for reg_component_id in clock_sensitivities {
                self.transfer(reg_component_id);
            }
        }
    }

    pub fn flow(&mut self) {
        while let Some(component_id) = self.dirty.pop() {
            if let Some(sensitivities) = self.sensitivities.get(&component_id) {
                for update_component_id in sensitivities.clone() {
                    let expr = self.exprs.get(&update_component_id).unwrap().clone();
                    let context = self.build_context(update_component_id, &expr);
                    let new_value = expr.eval(context);
                    if self.registers.contains(&update_component_id) {
                        self.set_reg(update_component_id, new_value);
                    } else {
                        self.set(update_component_id, new_value);
                    }
                }
            }
        }
    }

    pub fn resolve<S: AsRef<BStr>>(&mut self, path: S) -> ElaboratedComponentId {
        let elaboration = self.elaboration();
        elaboration.resolve(path).unwrap().id()
    }

    pub fn try_resolve<S: AsRef<BStr>>(&self, path: S) -> Option<ElaboratedComponentId> {
        self.elaboration().resolve(path).map(|c| c.id())
    }

    pub fn component_kind(&self, component_id: ElaboratedComponentId) -> ComponentKind {
        self.elaboration().component(component_id).component_kind()
    }

    pub fn component_type(&self, component_id: ElaboratedComponentId) -> Type {
        self.elaboration().component(component_id).typ()
    }

    pub fn full_name(&self, component_id: ElaboratedComponentId) -> BString {
        self.elaboration().component(component_id).path().clone()
    }

    /// Iterate over the direct children of `parent` (a path-prefix relation on the flat
    /// elaboration component list).  `parent_path` is the dotted path the children must
    /// extend by exactly one segment.  Yields `(id, leaf_name)` pairs.
    ///
    /// Use `children_of_root()` for the synthetic top scope (`"top"`), since the top
    /// module itself is not a component in the elaboration.
    pub fn children_of(
        &self,
        parent: ElaboratedComponentId,
    ) -> Vec<(ElaboratedComponentId, BString)> {
        let parent_path = self.elaboration().component(parent).path().clone();
        self.children_with_prefix(parent_path.as_slice())
    }

    pub fn children_of_root(&self) -> Vec<(ElaboratedComponentId, BString)> {
        self.children_with_prefix(b"top")
    }

    fn children_with_prefix(&self, prefix: &[u8]) -> Vec<(ElaboratedComponentId, BString)> {
        let elaboration = self.elaboration();
        let mut out = Vec::new();
        for comp in elaboration.components() {
            let path: &[u8] = comp.path().as_slice();
            if path.len() <= prefix.len() + 1 { continue; }
            if &path[..prefix.len()] != prefix { continue; }
            if path[prefix.len()] != b'.' { continue; }
            let rest = &path[prefix.len() + 1..];
            if rest.contains(&b'.') { continue; }
            out.push((comp.id(), BString::from(rest.to_vec())));
        }
        out
    }

    /// Build an eval `Context` for `component_id`'s driver expression.
    ///
    /// Each `Referent::Component(component_id)` in `expr` refers to a component by
    /// its pre-elaboration `ComponentId`.  We resolve it to an `ElaboratedComponentId`
    /// (and thus a current `Value`) by:
    ///   1. Looking up the component's relative path via its module's `ComponentAnalysis`.
    ///   2. Prepending the scope prefix of the elaborated component (the module instance
    ///      path from which references are resolved) to get the full elaborated path.
    ///   3. Resolving that path in the `Elaboration` to get the `ElaboratedComponentId`.
    ///   4. Reading the current value from `self.values`.
    fn build_context(&self, component_id: ElaboratedComponentId, expr: &Expr) -> Context {
        let elaboration = self.elaboration();
        let ec = elaboration.component(component_id);
        let prefix = scope_prefix(ec);

        let mut entries: Vec<(Referent, Value)> = Vec::new();
        for referent in collect_referents(expr) {
            if let Referent::Component(comp_id) = &referent {
                let comp_analysis = self.db.get_component_analysis(comp_id.item_id());
                if let Some(comp) = comp_analysis.component(*comp_id) {
                    let full_path: BString = format!("{prefix}.{}", comp.path()).into();
                    if let Some(elab_comp) = elaboration.resolve(&full_path) {
                        let value = self.values[&Node::val(elab_comp.id())].clone();
                        entries.push((referent, value));
                    }
                }
            }
        }
        Context::new(entries)
    }

    pub fn set(&mut self, component_id: ElaboratedComponentId, value: Value) {
        let node = Node::val(component_id);
        let value_ref = self.values.get_mut(&node).unwrap();
        *value_ref = value;
        self.dirty.insert(component_id);
    }

    fn set_reg(&mut self, component_id: ElaboratedComponentId, value: Value) {
        let node = Node::set(component_id);
        let value_ref = self.values.get_mut(&node).unwrap();
        *value_ref = value;
    }

    fn transfer(&mut self, component_id: ElaboratedComponentId) {
        let new_value = self.values.get_mut(&Node::set(component_id)).unwrap().clone();
        let value_ref = self.values.get_mut(&Node::val(component_id)).unwrap();
        *value_ref = new_value;
        self.dirty.insert(component_id);
    }

    pub fn get(&self, component_id: ElaboratedComponentId) -> Value {
        self.values.get(&Node::val(component_id)).unwrap().clone()
    }

    /// Current simulation time, in picoseconds.
    pub fn now(&self) -> u64 {
        self.sim_time_ps
    }

    /// Queue `event` to fire `delay_ps` after the current simulation time.
    /// Same-time events fire in registration (FIFO) order.
    pub fn schedule_after(&mut self, delay_ps: u64, event: Event) {
        let at_ps = self.sim_time_ps.saturating_add(delay_ps);
        let seq = self.next_seq;
        self.next_seq += 1;
        self.queue.push(Reverse(ScheduledEvent { at_ps, seq, event }));
    }

    /// Shorthand for `schedule_after(0, event)`: queue `event` to fire at
    /// the current simulation time, after any same-time events already
    /// in the queue (FIFO).  Defined in terms of `schedule_after` so the
    /// meaning stays stable when richer same-time semantics (Verilog
    /// regions) are added later.
    pub fn schedule(&mut self, event: Event) {
        self.schedule_after(0, event);
    }

    /// Drain the time queue.  Pops events in (time, seq) order, advances
    /// `sim_time_ps` to each event's deadline, and prints the event.
    /// Returns when the queue is empty.
    pub fn run(&mut self) -> Result<(), SimError> {
        while let Some(Reverse(s)) = self.queue.pop() {
            self.sim_time_ps = s.at_ps;
            println!("[t={}ps] event={:?}", s.at_ps, s.event);
        }
        Ok(())
    }
}

impl Node {
    fn val(elab_component_id: ElaboratedComponentId) -> Node {
        Node {
            component_id: elab_component_id,
            is_reg_set: false,
        }
    }

    fn set(elab_component_id: ElaboratedComponentId) -> Node {
        Node {
            component_id: elab_component_id,
            is_reg_set: true,
        }
    }
}

/// Build the data-flow dependency map for all elaborated components.
///
/// A component A depends on component B if B's path appears as a reference
/// inside A's driver expression(s).  References in drivers are relative to the
/// owning module's scope; this function converts them to absolute elaborated
/// paths before resolving.
pub fn build_dependencies(
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
