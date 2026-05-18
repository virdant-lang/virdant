use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};

use crate::analysis::elaboration::{Elaboration, SignalId};

use super::*;

/// Live circuit values and the propagation worklist.  Everything
/// mutated by combinational propagation, register transfers, and
/// direct `set` writes lives here.
///
/// Values are wrapped in `Arc` so that `Sim::build_context` can hand
/// the eval `Context` a refcount-bumped clone of each binding instead
/// of deep-copying every live signal value (a `Value::Ctor` carries
/// an owned `Vec<Value>`).
pub(super) struct State {
    vals: Vec<Arc<Value>>,
    prev_vals: Vec<Arc<Value>>,
    set_vals: IndexMap<SignalId, Arc<Value>>,
    dirty: IndexSet<SignalId>,
}

impl State {
    pub(super) fn new(elaboration: &Elaboration) -> State {
        let n_components = elaboration.components().len();
        let mut vals: Vec<Arc<Value>> = Vec::with_capacity(n_components);
        let mut prev_vals: Vec<Arc<Value>> = Vec::with_capacity(n_components);
        let mut set_vals: IndexMap<SignalId, Arc<Value>> = IndexMap::new();
        for elab_component in elaboration.components() {
            let elab_component_id = elab_component.id();
            let typ = elab_component.typ();

            vals.push(Arc::new(Value::X(typ.clone())));
            prev_vals.push(Arc::new(Value::X(typ.clone())));

            if elab_component.is_reg() {
                set_vals.insert(elab_component_id, Arc::new(Value::X(typ)));
            }
        }

        State {
            vals,
            prev_vals,
            set_vals,
            dirty: IndexSet::new(),
        }
    }

    pub(super) fn get(&self, signal_id: SignalId) -> Value {
        self.vals[signal_id.index()].as_ref().clone()
    }

    pub(super) fn get_prev(&self, signal_id: SignalId) -> Value {
        self.prev_vals[signal_id.index()].as_ref().clone()
    }

    pub(super) fn pop_dirty(&mut self) -> Option<SignalId> {
        self.dirty.pop()
    }

    pub(super) fn transfer(&mut self, component_id: SignalId) {
        let new_value = Arc::clone(self.set_vals.get(&component_id).unwrap());
        self.vals[component_id.index()] = new_value;
        self.dirty.insert(component_id);
    }

    /// Internal write: store `value` at `signal_id`'s val node and mark
    /// it dirty.  Does not flow, does not fire watchers, does not handle
    /// clock edges.  Called by `set` (the orchestrator) and by `flow`
    /// (during propagation, to avoid re-entrant orchestration).
    pub(super) fn set_val(&mut self, signal_id: SignalId, value: Value) {
        self.prev_vals[signal_id.index()] = Arc::clone(&self.vals[signal_id.index()]);
        self.vals[signal_id.index()] = Arc::new(value);
        self.dirty.insert(signal_id);
    }

    pub(super) fn set_reg(&mut self, signal_id: SignalId, value: Value) {
        let value_ref = self.set_vals.get_mut(&signal_id).unwrap();
        *value_ref = Arc::new(value);
    }
}
