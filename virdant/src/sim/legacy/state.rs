//! Live circuit values and the propagation worklist: `vals` for current
//! signal values, `set_vals` for pending register inputs, and the
//! `dirty` set driven by `flow`.
//!
//! There are no X or Z values: every cell is initialized by the sim's
//! `InitStrategy` (random by default), so even never-written registers
//! and top-level incoming ports carry fully-defined bit patterns.

use std::sync::Arc;

use indexmap::IndexSet;

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::sim::sim::InitStrategy;
use crate::sim::value::Value;

/// One bit per `SignalId::index()` word in `State::changed_signals`.
const CHANGE_WORD_BITS: usize = 64;

#[derive(Clone)]
struct MemoryState {
    /// One `Value` per declared word, shared through an `Arc` so the
    /// read path (which snapshots words into evaluation contexts)
    /// never copies the array.
    words: Arc<Vec<Value>>,
}

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
    /// `LegacySim::commit_domain`).
    pending_memory_writes: Vec<(SignalId, usize, Value)>,
    /// "Some watched signal's stored value changed since the last
    /// sweep pass that observed everything".  The watcher sweep can be
    /// skipped entirely when this is false: a watcher fires only on a
    /// value change (`Change`) or a 0→1 transition of bit 0
    /// (`ClockRising`), both require the observed signal's stored
    /// value to have changed, and the observed signal is watched by
    /// construction, so nothing can fire over a settlement in which no
    /// watched signal was written.  Set by `mark_changed` exactly when
    /// a write to a watched signal happens (unwatched writes leave
    /// this and the per-signal set untouched — see `watched`),
    /// consumed by each sweep pass together with the per-signal set
    /// (writes landing while the pass runs re-raise it for the next
    /// pass) and by `flow_all` (construction's full-network pass,
    /// before any watcher exists).  Not the same thing as `dirty`: the
    /// worklist drains during `flow`, so it is empty at dispatch time
    /// by construction and cannot serve as the predicate.
    changed_since_dispatch: bool,
    /// The per-signal detail of `changed_since_dispatch`: one bit per
    /// `SignalId::index()`, set by the same write primitives at the
    /// same time as the bool above (a write to a watched signal marks
    /// both or neither) and cleared together with it.  Each sweep pass
    /// of the facade's sweep-and-fire loop consumes the set at its
    /// start, after queueing the changed signals' buckets: the pass
    /// then visits exactly the watchers whose signals somebody
    /// actually wrote since the previous pass started — the settlement
    /// pass plus every callback-driven write since — and any write
    /// landing while the pass runs re-marks the set for the next pass.
    /// A watcher can only fire (or need its last-seen value refreshed)
    /// over a signal that was written, so visiting fewer buckets than
    /// the whole table changes nothing observable.
    changed_signals: Vec<u64>,
    /// The popcount of `changed_signals`, maintained incrementally by
    /// `mark_changed` (one increment per newly-marked signal) and reset
    /// with it.  O(1) size reads for the dispatch loop's quiescence
    /// check and the per-pass strategy switch — the loop compares the
    /// count against the watcher table two or three times per pass, so
    /// re-counting the bitset (one `count_ones` per word) would cost
    /// real time on wide designs.
    changed_count: usize,
    /// Whether any watcher observes each signal, indexed by
    /// `SignalId::index()`: the facade sets it at watcher registration
    /// and rebuilds it from the surviving table after pruning.  The
    /// changed-set bookkeeping in `mark_changed` is gated on this
    /// flag: the dispatch sweep visits only watchers, and a watcher's
    /// signal is watched by construction, so a write to an unwatched
    /// signal never needs a changed-set entry — on designs where most
    /// signals have no watcher (or none do at all), marking becomes a
    /// single flag read instead of bitset work on every write.
    watched: Vec<bool>,
}

impl State {
    pub(super) fn new(
        elaboration: &Elaboration,
        layouts: &crate::sim::layout::TypeLayoutIndex,
        strategy: &InitStrategy,
        memory_images: &[Option<crate::sim::circuit::MemoryCircuit>],
    ) -> State {
        let n_components = elaboration.components().len();
        let mut rng: u64 = strategy.rng_seed();
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
                // Draw order matters: two independent draws per
                // register (the live value above, the pending value
                // here), so seeded traces stay reproducible.
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
            watched: vec![false; n_components],
            changed_since_dispatch: false,
            changed_signals: vec![0; n_components.div_ceil(CHANGE_WORD_BITS)],
            changed_count: 0,
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

    /// Mark `signal_id` as committed for this settlement, returning
    /// whether it was newly marked (registers and memories commit at
    /// most once per settlement — see `transfer`).
    pub(super) fn mark_committed(&mut self, signal_id: SignalId) -> bool {
        self.committed.insert(signal_id)
    }

    /// Stage a memory write sampled at a clock edge: the write
    /// site's (enable, address, data) were evaluated against
    /// pre-edge values by phase 1 of the commit (`LegacySim::commit_domain`);
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
    /// commit.  Called by phase 2 (`LegacySim::commit_domain`) after the
    /// registers transferred, so the writes apply together with
    /// them.
    pub(super) fn drain_pending_memory_writes(
        &mut self,
    ) -> Vec<(SignalId, usize, Value)> {
        std::mem::take(&mut self.pending_memory_writes)
    }

    pub(super) fn transfer(&mut self, component_id: SignalId) {
        // At most one commit per register per settlement.  Inserting
        // even when the pending equals the current value matters: the
        // pending may be recomputed (with post-edge values) later in
        // the same settlement, and only the committed flag keeps that
        // recomputed pending from being committed by a late alias-edge
        // detection.
        if !self.committed.insert(component_id) {
            return;
        }
        let new_value = self.set_vals[component_id.index()]
            .as_ref()
            .expect("transfer: component has no pending register value")
            .clone();
        super::super::sim::debug_transfer(component_id, &new_value);
        if self.vals[component_id.index()] == new_value {
            return;
        }
        self.vals[component_id.index()] = new_value;
        self.mark_changed(component_id);
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
        self.mark_changed(signal_id);
        self.dirty.insert(signal_id);
    }

    pub(super) fn set_reg(&mut self, signal_id: SignalId, value: Value) {
        self.set_vals[signal_id.index()] = Some(value);
    }

    /// Record an actual write: queue it for `flow` and, for watched
    /// signals, raise both dispatch predicates.  The dirty insert is
    /// unconditional (propagation still needs every real write), but
    /// the changed set is touched only when the signal has a watcher:
    /// the dispatch sweep visits only watchers, and a watcher's signal
    /// is watched by construction, so a write to an unwatched signal
    /// can never need a changed-set entry.  For watched signals the
    /// bool and the bitset are set and cleared together, so the bool
    /// stays the O(1) "did any watched signal change" gate and the
    /// bitset the "which watched signals changed" detail the bucketed
    /// sweep dispatches over.
    fn mark_changed(&mut self, signal_id: SignalId) {
        let index = signal_id.index();
        if self.watched[index] {
            let word = index / CHANGE_WORD_BITS;
            let bit = 1 << (index % CHANGE_WORD_BITS);
            // Only a newly-marked signal grows the set: re-marking a
            // signal that is already pending must not double-count it.
            if self.changed_signals[word] & bit == 0 {
                self.changed_signals[word] |= bit;
                self.changed_count += 1;
            }
            self.changed_since_dispatch = true;
        }
    }
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

    /// True when some stored value changed since the last sweep that
    /// observed every watcher (or since construction): the predicate
    /// that lets the facade's `unlock` skip the watcher sweep entirely.
    pub(super) fn changed_since_dispatch(&self) -> bool {
        self.changed_since_dispatch
    }

    /// The number of signals with an actual write since the last
    /// `dispatch_pass` started (or since the last clear): the dispatch
    /// loop's quiescence check and the per-pass strategy switch.  When
    /// the changed set is smaller than the watcher table, the dispatch
    /// walks the changed signals' buckets; when it matches or exceeds
    /// the table (every register in a long chain changes each cycle),
    /// the dispatch walks the whole table directly — queueing a set of
    /// buckets no smaller than the table it replaces would only add
    /// cost.
    pub(super) fn changed_signal_count(&self) -> usize {
        self.changed_count
    }

    /// Iterate the watched signals with an actual write since the last
    /// `dispatch_pass` started (or since the last clear), in index
    /// order.  The per-signal predicate for the bucketed watcher
    /// sweep: a watcher fires only over a signal somebody actually
    /// wrote, and its last-seen value needs refreshing only then, so
    /// the sweep visits exactly these signals' buckets and no others.
    /// Unwatched signals are absent from the set by construction (see
    /// `watched`), which is exactly right: no watcher observes them.
    ///
    /// The walk skips directly between set bits (`trailing_zeros`),
    /// never scanning unset bits: the cost is proportional to the
    /// number of changed signals, not the size of the bitset.
    pub(super) fn changed_signal_ids(&self) -> impl Iterator<Item = SignalId> + '_ {
        self.changed_signals
            .iter()
            .enumerate()
            .flat_map(move |(word, &bits)| {
                let mut bits = bits;
                std::iter::from_fn(move || {
                    if bits == 0 {
                        None
                    } else {
                        let bit = bits.trailing_zeros();
                        bits &= !(1 << bit);
                        Some(SignalId::from_index(word * CHANGE_WORD_BITS + bit as usize))
                    }
                })
            })
    }

    /// Consume the changed-set: empties the per-signal bitset (and the
    /// whole-table skip flag, which tracks the same events).  A
    /// `dispatch_pass` calls this at its start, after queueing the
    /// changed signals' buckets: the pass then observes exactly those
    /// watchers, and any write that lands while it runs (watcher
    /// callbacks and their nested settlements) re-marks the set for the
    /// next pass.  `flow_all` calls it at construction (no watcher
    /// exists yet).  Any later write to a watched signal re-raises the
    /// flag and re-marks the signal's bit.
    pub(super) fn clear_changed_since_dispatch(&mut self) {
        self.changed_since_dispatch = false;
        self.changed_signals.iter_mut().for_each(|word| *word = 0);
        self.changed_count = 0;
    }

    /// Mark `signal_id` as observed by at least one watcher (or drop
    /// that claim): the facade sets it at registration and rebuilds
    /// the flags from the surviving table after pruning.  `mark_changed`
    /// gates the changed-set bookkeeping on this flag.
    pub(super) fn set_watched(&mut self, signal_id: SignalId, watched: bool) {
        self.watched[signal_id.index()] = watched;
    }

    /// Clear every watched flag: the prelude to a rebuild from the
    /// surviving watcher table after a prune.
    pub(super) fn clear_watched(&mut self) {
        self.watched.iter_mut().for_each(|flag| *flag = false);
    }
}
