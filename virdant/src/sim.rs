use std::sync::Arc;

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

pub mod expr;
pub mod payload;
pub mod eval;

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
/// of every `ExprReference` node encountered.
fn walk_expr_refs(parsing: &Parsing, root_id: AstNodeId) -> Vec<BString> {
    let mut result = Vec::new();
    let mut stack = vec![root_id];
    while let Some(id) = stack.pop() {
        let node = parsing.ast_node(id);
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(interned) = node.path() {
                result.push(parsing.string(interned).to_owned());
            }
        }
        for child in node.children() {
            stack.push(child.id());
        }
    }
    result
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
        ExprPayload::As(a)             => collect_referents_inner(&a.subject, out),
        // Leaves — no sub-expressions to recurse into.
        ExprPayload::BitLit(_)
        | ExprPayload::WordLit(_)
        | ExprPayload::StrLit(_)
        | ExprPayload::Enumerant(_)
        | ExprPayload::Hole(_)         => {}
    }
}

#[test]
#[rustfmt::skip]
fn test_sim() {
    let db = crate::util::db_from_file_with_lib("../examples/lfsr.vir", "../lib");
    crate::util::check_db(&db).unwrap();
    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"lfsr::Lfsr".into()).unwrap();

    let mut sim = Sim::new(Arc::new(db), top.id());
    let clock_id = sim.resolve(BStr::new(b"top.clock"));
    let reset_id = sim.resolve(BStr::new(b"top.reset"));
    let out_id = sim.resolve(BStr::new(b"top.out"));

    println!("--------------------------------------------------------------------------------");
    println!("Set reset = true and flow");
    sim.set(reset_id, Value::Bit(true));
    sim.flow();
    sim.dump();
    println!("--------------------------------------------------------------------------------");
    println!("tick clock");
    sim.tick(clock_id);
    sim.dump();
    println!("--------------------------------------------------------------------------------");
    println!("set reset to false and flow");
    sim.set(reset_id, Value::Bit(false));
    sim.flow();

    /*
     This is a classic 8-bit LFSR with:
       - State r initialized to 255 (0xFF)
       - Feedback bit = MSB (r[7])
       - Shift left by 1 (dropping MSB, inserting 0 at LSB)
       - If MSB was 1, XOR with taps = 29 = 0x1D = 0b00011101
    */
    const VALUES: &[u64] = &[
        255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110, 220, 165, 87, 174, 65, 130,
        25, 50, 100, 200, 141, 7, 14, 28, 56, 112, 224, 221, 167, 83, 166, 81, 162, 89, 178,
        121, 242, 249, 239, 195, 155, 43, 86, 172, 69, 138, 9, 18, 36, 72, 144, 61, 122, 244,
        245, 247, 243, 251, 235, 203, 139, 11, 22, 44, 88, 176, 125, 250, 233, 207, 131, 27,
        54, 108, 216, 173, 71, 142, 1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135,
        19, 38, 76, 152, 45, 90,
    ];

    for (cycle, expected) in VALUES.into_iter().enumerate() {
        let expected = Value::Word(8, *expected);
        let actual = sim.get(out_id);
        assert_eq!(expected, actual, "top.out had incorrect value on cycle {cycle}");
        sim.tick(clock_id);
    }
}
