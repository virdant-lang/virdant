//! Live circuit values and the propagation worklist: `vals` for current
//! signal values, `set_vals` for pending register inputs, and the
//! `dirty` set driven by `flow`.
//!
//! There are no X or Z values: every cell is initialized by the sim's
//! `InitStrategy` (random by default), so even never-written registers
//! and top-level incoming ports carry fully-defined bit patterns.

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::sim::sim::InitStrategy;

use super::dirty::DirtyList;

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
    dirty: DirtyList,
    /// Settlement-level "some stored value changed since the last
    /// watcher sweep that observed everything".  The watcher sweep can
    /// be skipped entirely when this is false: a watcher fires only on
    /// a value change (`Change`) or a 0→1 transition of bit 0
    /// (`ClockRising`), and both require the signal's stored value to
    /// have changed, so nothing can fire over values nobody wrote to.
    /// Set by the two write primitives below exactly when a write
    /// happens (they already detect change internally), cleared by the
    /// outermost `unlock` after its quiescent sweep pass and by
    /// `flow_all` (construction's full-network pass, before any
    /// watcher exists).  Not the same thing as `dirty`: the worklist
    /// drains during `flow`, so it is empty at dispatch time by
    /// construction and cannot serve as the predicate.
    changed_since_dispatch: bool,
}

impl State {
    pub(super) fn new(
        elaboration: &Elaboration,
        layouts: &crate::sim::layout::TypeLayoutIndex,
        strategy: &InitStrategy,
    ) -> State {
        let n_components = elaboration.components().len();
        let mut rng: u64 = strategy.rng_seed();
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
            dirty: DirtyList::new(n_components),
            changed_since_dispatch: false,
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
        let popped = self.dirty.pop();
        #[cfg(feature = "sim-profile")]
        if popped.is_some() {
            crate::sim::profile::bump_dirty_pop();
        }
        popped
    }

    pub(super) fn transfer(&mut self, component_id: SignalId) {
        // Timed (sampled) at the `transfer_clock` rising-edge loop
        // rather than per call: a ~26 ns `Instant` read would distort
        // a ~5 ns transfer (see `sim::profile`).  Calls are counted in
        // bulk by the caller; only actual writes are counted here.
        let new_value = self.set_vals[component_id.index()]
            .as_ref()
            .expect("transfer: component has no pending register value")
            .clone();
        super::sim::debug_transfer(component_id, &new_value);
        if self.vals[component_id.index()] == new_value {
            return;
        }
        self.vals[component_id.index()] = new_value;
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_transfer_write();
        self.dirty.insert(component_id);
        self.changed_since_dispatch = true;
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
        // Sampled timing, not per-call `Instant`: same per-driver-path
        // rule as `transfer` above (see `sim::profile`).  The tick
        // also counts the call.
        #[cfg(feature = "sim-profile")]
        let _profile_sample = crate::sim::profile::sample_set_val();
        if self.vals[signal_id.index()] == value {
            return;
        }
        self.vals[signal_id.index()] = value;
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_set_val_write();
        self.dirty.insert(signal_id);
        self.changed_since_dispatch = true;
    }

    pub(super) fn set_reg(&mut self, signal_id: SignalId, value: Value) {
        self.set_vals[signal_id.index()] = Some(value);
    }
}

impl State {
    pub(super) fn clear_dirty(&mut self) {
        self.dirty.clear();
    }

    /// True when some stored value changed since the last sweep that
    /// observed every watcher (or since construction): the predicate
    /// that lets `unlock` skip the watcher sweep entirely.
    pub(super) fn changed_since_dispatch(&self) -> bool {
        self.changed_since_dispatch
    }

    /// Record that the sweep has observed everything: only correct
    /// immediately after a quiescent `dispatch_pass` (every watcher's
    /// `last` was brought up to the live value and no callback ran) or
    /// after `flow_all` (no watcher exists yet).  Any later actual
    /// write re-raises the flag.
    pub(super) fn clear_changed_since_dispatch(&mut self) {
        self.changed_since_dispatch = false;
    }
}

/// The executor's view of engine state: raw bits out of the live
/// value tables.  Narrow reads never allocate (`Value::Small` is an
/// inline `u64`); wide reads clone the `BitPat`, as the interpreter
/// did for Big values.
impl crate::sim::vm::VmState for State {
    fn narrow_bits(&self, signal_id: SignalId) -> u64 {
        self.get(signal_id).to_u64()
    }

    fn wide_bits(&self, signal_id: SignalId) -> crate::sim::bits::BitPat {
        self.get(signal_id).bitpat()
    }
}
