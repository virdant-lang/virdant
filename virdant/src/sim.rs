use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::{IndexMap, IndexSet};

use crate::analysis::symbols::SymbolId;
use crate::analysis::Location;
use crate::analysis::drivers::Driver;
use crate::analysis::elaboration::{ElaboratedComponent, ElaboratedComponentId, Elaboration};
use crate::common::ComponentKind;
use crate::db::Db;
use crate::sim::eval::Value;
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
}

impl Sim {
    pub fn new(db: Arc<Db>, top: SymbolId) -> Sim {
        let elaboration = db.get_elaboration(top);

        let deps = build_dependencies(&db, &elaboration);
        let sensitivities = build_sensitivities(&deps);
        let clock_sensitivities = build_clock_sensitivities(&elaboration);

        let mut sim = Sim {
            values: IndexMap::new(),
            component_paths: IndexMap::new(),
            db: db.clone(),
            top,
            deps,
            sensitivities,
            clock_sensitivities,
            dirty: IndexSet::new(),
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
            }
        }

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
        let path = self.component_paths.get(&component_id).unwrap();
        println!("TICK: {:?}", path);
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

        self.dirty.clear();
    }

    pub fn resolve<S: AsRef<BStr>>(&mut self, path: S) -> ElaboratedComponentId {
        let elaboration = self.elaboration();
        elaboration.resolve(path).unwrap().id()
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

#[test]
fn test_sim() {
    let db = crate::util::db_from_dir_with_lib("../examples/gcd/src", "../lib");
    crate::util::check_db(&db).unwrap();
    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"top::Top".into()).unwrap();

    let mut sim = Sim::new(Arc::new(db), top.id());
    let clock_id = sim.resolve(BStr::new(b"top.clock"));
    let reset_id = sim.resolve(BStr::new(b"top.reset"));

    println!("--------------------------------------------------------------------------------");
    sim.dump();

    println!("--------------------------------------------------------------------------------");
    sim.set(reset_id, Value::Bit(true));
    sim.dump();


    println!("--------------------------------------------------------------------------------");
    sim.dump();
    let clock_id = sim.resolve(BStr::new(b"top.clock"));
    sim.tick(clock_id);
    println!("--------------------------------------------------------------------------------");
    sim.dump();
}
