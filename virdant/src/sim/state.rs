//! Live circuit values and the propagation worklist: `vals` for current
//! signal values, `set_vals` for pending register inputs, and the
//! `dirty` set driven by `flow`.
//!
//! There are no X or Z values: every cell is initialized by the sim's
//! `InitStrategy` (random by default), so even never-written registers
//! and top-level incoming ports carry fully-defined bit patterns.

use indexmap::IndexSet;

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::sim::sim::InitStrategy;

use super::*;

/// Live circuit values and the propagation worklist.  Everything
/// mutated by combinational propagation, register transfers, and
/// direct `set` writes lives here.
///
/// Values are stored inline: with interned type tags a `Value` is a
/// small struct for the common (≤64-bit) case, so storing, cloning,
/// and comparing one never touches the heap.
pub(super) struct State {
    vals: Vec<Value>,
    /// Pending next-values for registers, densely indexed by
    /// `SignalId::index()` (`None` for non-registers).
    set_vals: Vec<Option<Value>>,
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
        let mut vals: Vec<Value> = Vec::with_capacity(n_components);
        let mut set_vals: Vec<Option<Value>> = vec![None; n_components];
        for elab_component in elaboration.components() {
            let elab_component_id = elab_component.id();
            let typ = elab_component.typ();
            let typ_id = layouts.type_id(&typ);
            let width = layouts.type_width(&typ);

            vals.push(strategy.value(typ_id, width, &mut rng));

            if elab_component.is_reg() {
                set_vals[elab_component_id.index()] =
                    Some(strategy.value(typ_id, width, &mut rng));
            }
        }

        State {
            vals,
            set_vals,
            dirty: IndexSet::new(),
        }
    }

    pub(super) fn get(&self, signal_id: SignalId) -> Value {
        self.vals[signal_id.index()].clone()
    }

    /// Borrow the live value of `signal_id` without cloning it (for
    /// allocation-free watcher-sweep comparisons; clone only on an
    /// actual change).
    pub(super) fn peek(&self, signal_id: SignalId) -> &Value {
        &self.vals[signal_id.index()]
    }

    pub(super) fn pop_dirty(&mut self) -> Option<SignalId> {
        self.dirty.pop()
    }

    pub(super) fn transfer(&mut self, component_id: SignalId) {
        let new_value = self.set_vals[component_id.index()]
            .as_ref()
            .expect("transfer: component has no pending register value")
            .clone();
        super::sim::debug_transfer(component_id, &new_value);
        if self.vals[component_id.index()] == new_value {
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
    /// one propagation pass — re-marking an unchanged signal dirty would
    /// cycle the worklist forever.
    pub(super) fn set_val(&mut self, signal_id: SignalId, value: Value) {
        if self.vals[signal_id.index()] == value {
            return;
        }
        self.vals[signal_id.index()] = value;
        self.dirty.insert(signal_id);
    }

    pub(super) fn set_reg(&mut self, signal_id: SignalId, value: Value) {
        self.set_vals[signal_id.index()] = Some(value);
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
    pub(super) fn clear_dirty(&mut self) {
        self.dirty.clear();
    }
}
