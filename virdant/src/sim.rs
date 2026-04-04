use std::sync::Arc;

use bstr::{BStr, BString};
use indexmap::IndexMap;

use crate::analysis::symbols::SymbolId;
use crate::db::Db;
use crate::sim::eval::Value;
use crate::analysis::elaboration::{ElaboratedComponentId, Elaboration};

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
}

impl Sim {
    pub fn new(db: Arc<Db>, top: SymbolId) -> Sim {
        let mut sim = Sim {
            values: IndexMap::new(),
            component_paths: IndexMap::new(),
            db: db.clone(),
            top,
        };

        for elab_component in db.get_elaboration(top).components() {
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
        for (node, value) in &self.values {
            let path = &self.component_paths.get(&node.component_id).unwrap();
            if node.is_reg_set {
                let elaboration = self.elaboration();
                let clock_id = elaboration.component(node.component_id).clock().unwrap();
                let clock = elaboration.component(clock_id);
                println!("{path} <= {value:?} on {:?}", clock.path());
            } else {
                println!("{path} := {value:?}");
            }
        }
    }

    pub fn tick(&mut self) {
        todo!()
    }

    pub fn flow(&mut self) {
        todo!()
    }

    pub fn set<S: AsRef<BStr>>(&mut self, path: S, value: Value) {
        let elaboration = self.elaboration();
        let component_id = elaboration.resolve(path).unwrap().id();
        let node = Node::val(component_id);
        let value_ref = self.values.get_mut(&node).unwrap();
        *value_ref = value;
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

#[test]
fn test_sim() {
    let db = crate::util::db_from_dir_with_lib("../examples/gcd/src", "../lib");
    crate::util::check_db(&db).unwrap();
    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"top::Top".into()).unwrap();

    let mut sim = Sim::new(Arc::new(db), top.id());
    sim.dump();
    sim.set(BStr::new(b"top.gcd.state"), Value::Bit(true));

    println!("--------------------------------------------------------------------------------");
    sim.dump();
}
