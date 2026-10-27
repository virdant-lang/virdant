//! Live circuit values and the propagation worklist: `vals`/`prev_vals`
//! for current/previous signal values, `set_vals` for pending register
//! inputs, and the `dirty` set driven by `flow`, with `Arc`-wrapped
//! values shared by eval contexts.
//!
//! There are no X or Z values: every cell is initialized by the sim's
//! `InitStrategy` (random by default), so even never-written registers
//! and top-level incoming ports carry fully-defined bit patterns.

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
/// of deep-copying every live signal value.
pub(super) struct State {
    vals: Vec<Arc<Value>>,
    prev_vals: Vec<Arc<Value>>,
    set_vals: IndexMap<SignalId, Arc<Value>>,
    dirty: IndexSet<SignalId>,
}

impl State {
    pub(super) fn new(
        elaboration: &Elaboration,
        layouts: &crate::sim::layout::TypeLayoutIndex,
        strategy: &InitStrategy,
    ) -> State {
        let n_components = elaboration.components().len();
        let mut rng: u64 = initial_seed();
        let mut vals: Vec<Arc<Value>> = Vec::with_capacity(n_components);
        let mut prev_vals: Vec<Arc<Value>> = Vec::with_capacity(n_components);
        let mut set_vals: IndexMap<SignalId, Arc<Value>> = IndexMap::new();
        for elab_component in elaboration.components() {
            let elab_component_id = elab_component.id();
            let typ = elab_component.typ();
            let width = layouts.type_width(&typ);

            vals.push(Arc::new(strategy.value(typ.clone(), width, &mut rng)));
            prev_vals.push(Arc::new(strategy.value(typ.clone(), width, &mut rng)));

            if elab_component.is_reg() {
                set_vals.insert(elab_component_id, Arc::new(strategy.value(typ.clone(), width, &mut rng)));
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
        super::sim::debug_transfer(component_id, &new_value);
        if self.vals[component_id.index()].as_ref() == new_value.as_ref() {
            return;
        }
        self.vals[component_id.index()] = new_value;
        self.dirty.insert(component_id);
    }

    /// Internal write: store `value` at `signal_id`'s val node and mark
    /// it dirty.  Does not flow, does not fire watchers, does not handle
    /// clock edges.  Called by `set` (the orchestrator) and by `flow`
    /// (during propagation, to avoid re-entrant orchestration).
    ///
    /// Idempotent: writing the current value is a no-op.  This matters
    /// for `flow`, which may evaluate a component several times within
    /// one propagation pass — `prev_vals` must keep the *pre-pass* value
    /// so `unlock`'s change detection sees the net transition (writing
    /// same-value repeatedly would otherwise erase the change history
    /// and starve watchers/VCD of real transitions).
    pub(super) fn set_val(&mut self, signal_id: SignalId, value: Value) {
        if self.vals[signal_id.index()].as_ref() == &value {
            return;
        }
        self.prev_vals[signal_id.index()] = Arc::clone(&self.vals[signal_id.index()]);
        self.vals[signal_id.index()] = Arc::new(value);
        self.dirty.insert(signal_id);
    }

    pub(super) fn set_reg(&mut self, signal_id: SignalId, value: Value) {
        let value_ref = self.set_vals.get_mut(&signal_id).unwrap();
        *value_ref = Arc::new(value);
    }

    /// Acknowledge that the current value of `signal_id` has been observed
    /// (e.g. an `on_change` watcher just fired for it).  Synchronises
    /// `prev_vals` with `vals` so subsequent change-detection passes see
    /// "no change" until the signal is written again.
    pub(super) fn commit_change(&mut self, signal_id: SignalId) {
        self.prev_vals[signal_id.index()] = Arc::clone(&self.vals[signal_id.index()]);
    }
}

/// A seed for the random init strategy, derived from the wall clock so
/// two runs see different uninitialized values.
fn initial_seed() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0x9E3779B97F4A7C15)
}

impl State {
    /// Drop the pending worklist without propagating (used after a
    /// full-network evaluation pass).
    pub(super) fn dirty_len(&self) -> usize {
        self.dirty.len()
    }

    pub(super) fn clear_dirty(&mut self) {
        self.dirty.clear();
    }
}
