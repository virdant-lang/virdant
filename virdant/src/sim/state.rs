//! Live circuit values and the propagation worklist: `vals` for current
//! signal values, `set_vals` for pending register inputs,
//! `pending_memory_writes` for memory writes staged at a clock edge,
//! and the `dirty` set driven by `flow`.
//!
//! There are no X or Z values: every cell is initialized by the sim's
//! `InitStrategy` (random by default), so even never-written registers
//! and top-level incoming ports carry fully-defined bit patterns.

use std::sync::Arc;

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
    /// Memory words, densely indexed by `SignalId::index()`;
    /// `None` for non-memory components.  A memory's `vals` entry is
    /// not its contents — it is a version counter bumped by every
    /// committed write so combinational readers re-evaluate (the
    /// dirty-propagation channel for `mem[dyn i]` reads).
    memories: Vec<Option<MemoryState>>,
    /// Pending next-values for registers, densely indexed by
    /// `SignalId::index()` (`None` for non-registers).
    set_vals: Vec<Option<Value>>,
    dirty: IndexSet<SignalId>,
    /// Registers already transferred during the current settlement's
    /// clock-edge handling.  A register commits at most once per
    /// settlement: when the source clock rises directly (`set`), its
    /// alias copies (a submodule clock port driven by a bare reference
    /// or a socket connection) rise again later in the same `flow` —
    /// by which time the register's pending has already been
    /// recomputed with post-edge values, and committing it a second
    /// time would clobber the value sampled at the edge.
    committed: IndexSet<SignalId>,
    /// Memory writes staged by phase 1 of a clock-edge commit:
    /// `(memory, index, value)` triples sampled against pre-edge
    /// values.  Applied (drained) by phase 2 once the whole clock
    /// domain has been staged, so a memory write site never sees a
    /// register transferred at the same edge (see
    /// `Sim::commit_domain`).
    pending_memory_writes: Vec<(SignalId, usize, Value)>,
}

impl State {
    pub(super) fn new(
        elaboration: &Elaboration,
        layouts: &crate::sim::layout::TypeLayoutIndex,
        strategy: &InitStrategy,
        memory_images: &[Option<crate::sim::circuit::MemoryCircuit>],
    ) -> State {
        let n_components = elaboration.components().len();
        let mut rng: u64 = initial_seed();
        let mut vals: Vec<Value> = Vec::with_capacity(n_components);
        let mut set_vals: Vec<Option<Value>> = vec![None; n_components];
        let mut memories: Vec<Option<MemoryState>> = vec![None; n_components];
        for elab_component in elaboration.components() {
            let elab_component_id = elab_component.id();
            let typ = elab_component.typ();
            let typ_id = layouts.type_id(&typ);
            let width = layouts.type_width(&typ);

            vals.push(strategy.value(typ_id, width, &mut rng));

            if let Some(memory) = &memory_images[elab_component_id.index()] {
                // One word per declared depth: image words where the
                // image provides them, pseudorandom otherwise
                // (never-written words read as unspecified).
                let words: Vec<Value> = (0..memory.depth)
                    .map(|index| {
                        memory.image
                            .get(index as usize)
                            .and_then(|word| word.clone())
                            .unwrap_or_else(|| strategy.value(typ_id, width, &mut rng))
                    })
                    .collect();
                memories[elab_component_id.index()] = Some(MemoryState {
                    words: Arc::new(words),
                });
            }

            if elab_component.is_reg() {
                set_vals[elab_component_id.index()] =
                    Some(strategy.value(typ_id, width, &mut rng));
            }
        }

        State {
            vals,
            memories,
            set_vals,
            dirty: IndexSet::new(),
            committed: IndexSet::new(),
            pending_memory_writes: Vec::new(),
        }
    }

    /// The words of memory `signal_id` (shared cheaply through an
    /// `Arc`; the read path never copies the array).
    pub(super) fn memory_words(&self, signal_id: SignalId) -> Arc<Vec<Value>> {
        self.memories[signal_id.index()]
            .as_ref()
            .expect("memory_words: not a memory")
            .words
            .clone()
    }

    /// Store `value` at `index` of memory `signal_id`, returning
    /// whether the word changed.  A change bumps the memory's version
    /// counter (`vals`) and marks it dirty, so combinational readers
    /// re-evaluate after the clock-edge commit.
    pub(super) fn set_memory_word(
        &mut self,
        signal_id: SignalId,
        index: usize,
        value: Value,
    ) -> bool {
        let words = match self.memories[signal_id.index()].as_mut() {
            Some(MemoryState { words }) => Arc::make_mut(words),
            None => panic!("set_memory_word: not a memory"),
        };
        if let Some(word) = words.get_mut(index) {
            if *word == value {
                return false;
            }
            *word = value;
        }
        // Bump the version counter (any visible change works; the
        // counter is only a change detector).
        let old = self.vals[signal_id.index()].clone();
        let bits = old.to_u64().wrapping_add(1);
        let new = Value::from_u64(old.typ_id(), old.width(), bits);
        self.vals[signal_id.index()] = new;
        self.dirty.insert(signal_id);
        true
    }

    /// Stage a memory write sampled at a clock edge: the write
    /// site's (enable, address, data) were evaluated against
    /// pre-edge values by phase 1 of the commit (`Sim::commit_domain`);
    /// the word itself is only updated when phase 2 drains the
    /// staged writes, after every register of the domain has
    /// transferred.
    pub(super) fn stage_memory_write(
        &mut self,
        signal_id: SignalId,
        index: usize,
        value: Value,
    ) {
        self.pending_memory_writes.push((signal_id, index, value));
    }

    /// Take every memory write staged for the current clock-edge
    /// commit.  Called by phase 2 (`Sim::commit_domain`) after the
    /// registers transferred, so the writes apply together with
    /// them.
    pub(super) fn drain_pending_memory_writes(
        &mut self,
    ) -> Vec<(SignalId, usize, Value)> {
        std::mem::take(&mut self.pending_memory_writes)
    }

    /// Mark `signal_id` as committed for this settlement, returning
    /// whether it was newly marked (registers and memories commit at
    /// most once per settlement — see `transfer`).
    pub(super) fn mark_committed(&mut self, signal_id: SignalId) -> bool {
        self.committed.insert(signal_id)
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
        // At most one commit per register per settlement.  Inserting
        // even when the pending equals the current value matters: the
        // pending may be recomputed (with post-edge values) later in
        // the same settlement, and only the committed flag keeps that
        // recomputed pending from being committed by a late alias-edge
        // detection.
        if !self.mark_committed(component_id) {
            return;
        }
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

/// The live contents of one memory.
#[derive(Debug, Clone)]
struct MemoryState {
    /// One `Value` per declared word, shared through an `Arc` so the
    /// read path (which snapshots words into evaluation contexts)
    /// never copies the array.
    words: Arc<Vec<Value>>,
}

impl State {
    pub(super) fn clear_dirty(&mut self) {
        self.dirty.clear();
    }

    /// Reset the per-settlement commit markers.  Called when a
    /// settlement's `flow` reaches quiescence, so the next settlement
    /// (with its own clock edges) starts fresh.
    pub(super) fn clear_committed(&mut self) {
        self.committed.clear();
    }
}
