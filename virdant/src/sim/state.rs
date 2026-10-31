//! Live circuit values and the propagation worklist: `vals` for current
//! signal values, `set_vals` for pending register inputs, and the
//! `dirty` set driven by `flow`.
//!
//! There are no X or Z values: every cell is initialized by the sim's
//! `InitStrategy` (random by default), so even never-written registers
//! and top-level incoming ports carry fully-defined bit patterns.

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::common::Width;

/// One bit per `SignalId::index()` word in `State::changed_signals`.
const CHANGE_WORD_BITS: usize = 64;
use crate::sim::layout::TypeId;
use crate::sim::sim::InitStrategy;

use super::dirty::DirtyList;

use super::*;

/// A register's pending next-value (`State::set_vals`).  Narrow
/// pending values store raw bits plus the tag (`TypeId`/width) the
/// committed `Value` will carry, so the transfer loop compares raw
/// integers against the stored bits and tags a `Value` only on an
/// actual write (PERF-IDEAS item 10); wide pending values keep the
/// `Value` representation.
#[derive(Clone, Debug)]
enum Pending {
    /// Not a register: no pending next-value.
    None,
    /// A narrow (<= 64-bit) pending next-value: raw bits (masked to
    /// `width`) plus the type tag and width of the committed `Value`.
    Bits { typ: TypeId, width: Width, bits: u64 },
    /// A wide pending next-value.
    Value(Value),
}

impl Pending {
    /// Decompose a pending `Value`: narrow values become raw bits
    /// (the transfer loop compares raw integers against them), wide
    /// ones stay as-is.
    fn from_value(value: Value) -> Pending {
        match value.as_narrow() {
            Some((width, bits)) => Pending::Bits { typ: value.typ_id(), width, bits },
            None => Pending::Value(value),
        }
    }
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
    /// Pending next-values for registers, densely indexed by
    /// `SignalId::index()` (`Pending::None` for non-registers).
    set_vals: Vec<Pending>,
    dirty: DirtyList,
    /// "Some watched signal's stored value changed since the last
    /// sweep pass that observed everything".  The watcher sweep can be
    /// skipped entirely when this is false: a watcher fires only on a
    /// value change (`Change`) or a 0→1 transition of bit 0
    /// (`ClockRising`), both require the observed signal's stored
    /// value to have changed, and the observed signal is watched by
    /// construction, so nothing can fire over a settlement in which no
    /// watched signal was written.  Set by the two write primitives
    /// below exactly when a write to a watched signal happens (they
    /// already detect change internally; unwatched writes leave this
    /// and the per-signal set untouched — see `watched`), consumed by
    /// each sweep pass together with the per-signal set (writes
    /// landing while the pass runs re-raise it for the next pass) and
    /// by `flow_all` (construction's full-network pass, before any
    /// watcher exists).  Not the same thing as `dirty`: the worklist
    /// drains during `flow`, so it is empty at dispatch time by
    /// construction and cannot serve as the predicate.
    changed_since_dispatch: bool,
    /// The per-signal detail of `changed_since_dispatch`: one bit per
    /// `SignalId::index()`, set by the same write primitives at the
    /// same time as the bool above (a write to a watched signal marks
    /// both or neither) and cleared together with it.  Each sweep pass of the unlock's
    /// sweep-and-fire loop consumes the set at its start, after
    /// queueing the changed signals' buckets: the pass then visits
    /// exactly the watchers whose signals somebody actually wrote
    /// since the previous pass started — the settlement pass plus
    /// every callback-driven write since — and any write landing while
    /// the pass runs re-marks the set for the next pass.  A watcher
    /// can only fire (or need its last-seen value refreshed) over a
    /// signal that was written, so visiting fewer buckets than the
    /// whole table changes nothing observable.
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
    /// `SignalId::index()`: `Sim` sets it at watcher registration and
    /// rebuilds it from the surviving table after pruning.  The
    /// changed-set bookkeeping in `mark_changed` is gated on this
    /// flag: the dispatch sweep visits only watchers, and a watcher's
    /// signal is watched by construction, so a write to an unwatched
    /// signal never needs a changed-set entry — on designs where most
    /// signals have no watcher (or none do at all), marking becomes a
    /// single flag read instead of bitset work on every write.
    watched: Vec<bool>,
    /// `epochs[signal]`: the value of `epoch_clock` at the signal's
    /// most recent real write (0 = never written since construction).
    /// Bumped by `mark_changed`, the one funnel every real write goes
    /// through, so the counter total-orders every stored-value change.
    /// This is the flow-time change record: a repeat visit whose
    /// trigger epoch predates a driver's last evaluation implies the
    /// driver already observed that write (see `vm::schedule`).
    epochs: Vec<u64>,
    /// Monotonic write counter backing `epochs`: incremented once per
    /// real write, in write order.
    epoch_clock: u64,
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
        let mut set_vals: Vec<Pending> = vec![Pending::None; n_components];
        for elab_component in elaboration.components() {
            let elab_component_id = elab_component.id();
            let typ = elab_component.typ();
            let typ_id = layouts.type_id(&typ);
            let width = layouts.type_width(&typ);

            vals.push(strategy.value(typ_id, width, &mut rng));

            if elab_component.is_reg() {
                // Draw order matters: two independent draws per
                // register (the live value above, the pending value
                // here), so seeded traces stay reproducible.
                let pending = strategy.value(typ_id, width, &mut rng);
                set_vals[elab_component_id.index()] = Pending::from_value(pending);
            }
        }

        State {
            vals,
            set_vals,
            dirty: DirtyList::new(n_components),
            watched: vec![false; n_components],
            changed_since_dispatch: false,
            changed_signals: vec![0; n_components.div_ceil(CHANGE_WORD_BITS)],
            changed_count: 0,
            epochs: vec![0; n_components],
            epoch_clock: 0,
        }
    }

    /// The narrow-value store's base address, for the JIT's direct
    /// state reads (`sim::vm::jit`; see `value::Value::jit_layout` for
    /// the stride/offset invariants).  `State::vals` is allocated at
    /// exactly `n_components` in `State::new` and afterwards only ever
    /// written in place — it never reallocates — so the compiled phase
    /// code can bake this pointer in for the engine's whole lifetime.
    /// Read-only: the compiled code never writes engine state.
    #[cfg(feature = "jit")]
    pub(super) fn jit_vals_base(&self) -> *const u8 {
        self.vals.as_ptr() as *const u8
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

    /// The number of signals currently in the propagation worklist:
    /// the settlement-size gate reads it once at `flow` entry.
    pub(super) fn dirty_len(&self) -> usize {
        self.dirty.len()
    }

    /// The worklist's insertion-order signal-index sequence: the
    /// settlement's dirty-set signature (the cached straight-line
    /// schedule's key).
    pub(super) fn dirty_stack(&self) -> &[u32] {
        self.dirty.stack()
    }

    /// The settlement's dirty-set bitmap (one bit per signal index,
    /// full worklist width): read at `flow` entry, where it is exactly
    /// the settlement's dirty set.  The deep-cycle replay's soundness
    /// key — see `vm::schedule::CycleSchedule`.
    pub(super) fn dirty_bits(&self) -> &[u64] {
        self.dirty.bits()
    }

    pub(super) fn pop_dirty(&mut self) -> Option<SignalId> {
        let popped = self.dirty.pop();
        #[cfg(feature = "sim-profile")]
        if popped.is_some() {
            crate::sim::profile::bump_dirty_pop();
        }
        popped
    }


    /// The epoch of the signal at component index `index` (the static
    /// flow path works in indexes): the global write counter's value at
    /// the signal's most recent real write, 0 if never written.  See
    /// `epochs`.
    pub(super) fn epoch_at(&self, index: usize) -> u64 {
        self.epochs[index]
    }

    /// The current value of the global write counter.  A driver's
    /// last-eval snapshot is taken from this immediately before the
    /// driver runs: any input written later carries a larger epoch and
    /// forces re-evaluation; any input written earlier is already
    /// reflected in the output the driver committed.
    pub(super) fn epoch_clock(&self) -> u64 {
        self.epoch_clock
    }

    pub(super) fn transfer(&mut self, component_id: SignalId) -> bool {
        // Returns whether the live value was actually written: the
        // flow loop expands a driver's fanout only on a real
        // write.
        // Timed (sampled) at the `transfer_clock` rising-edge loop
        // rather than per call: a ~26 ns `Instant` read would distort
        // a ~5 ns transfer (see `sim::profile`).  Calls are counted in
        // bulk by the caller; only actual writes are counted here.
        match &self.set_vals[component_id.index()] {
            Pending::Bits { typ, width, bits } => {
                // Narrow transfer (PERF-IDEAS item 10): compare raw
                // integers against the stored bits and construct the
                // pending `Value` only on an actual write.  The raw
                // compare is exact against a narrow stored value of
                // the same width and tag; any other stored shape falls
                // back to the tagged `Value` compare, so the
                // dirty/changed protocol sees exactly the same change
                // events as the `Value` path.
                let stored = &self.vals[component_id.index()];
                let unchanged = match stored.as_narrow() {
                    Some((stored_width, stored_bits)) => {
                        stored_width == *width && stored.typ_id() == *typ && stored_bits == *bits
                    }
                    None => *stored != Value::from_u64(*typ, *width, *bits),
                };
                if unchanged {
                    return false;
                }
                let new_value = Value::from_u64(*typ, *width, *bits);
                super::sim::debug_transfer(component_id, &new_value);
                self.vals[component_id.index()] = new_value;
                #[cfg(feature = "sim-profile")]
                crate::sim::profile::bump_transfer_write();
                self.mark_changed(component_id);
                true
            }
            Pending::Value(pending) => {
                let new_value = pending.clone();
                super::sim::debug_transfer(component_id, &new_value);
                if self.vals[component_id.index()] == new_value {
                    return false;
                }
                self.vals[component_id.index()] = new_value;
                #[cfg(feature = "sim-profile")]
                crate::sim::profile::bump_transfer_write();
                self.mark_changed(component_id);
                true
            }
            Pending::None => panic!("transfer: component has no pending register value"),
        }
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
    pub(super) fn set_val(&mut self, signal_id: SignalId, value: Value) -> bool {
        // Sampled timing, not per-call `Instant`: same per-driver-path
        // rule as `transfer` above (see `sim::profile`).  The tick
        // also counts the call.
        #[cfg(feature = "sim-profile")]
        let _profile_sample = crate::sim::profile::sample_set_val();
        if self.vals[signal_id.index()] == value {
            return false;
        }
        self.vals[signal_id.index()] = value;
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_set_val_write();
        self.mark_changed(signal_id);
        true
    }

    /// The bits-only write primitive (PERF-IDEAS item 10):
    /// `set_val`'s raw-integer twin for narrow drivers.  The compare
    /// is against the stored value's raw bits and the tagged `Value`
    /// is constructed only on an actual write.  The raw compare is
    /// exact against a narrow stored value of the same width and tag;
    /// any other stored shape (wide value, width or tag mismatch)
    /// falls back to the tagged `Value` compare, so the dirty-list
    /// protocol and the `changed_since_dispatch` flag see exactly the
    /// same change events as `set_val`.
    pub(super) fn set_val_bits(&mut self, signal_id: SignalId, typ: TypeId, width: Width, bits: u64) -> bool {
        // Sampled timing, not per-call `Instant`: same per-driver-path
        // rule as `set_val` above (see `sim::profile`).  The tick
        // also counts the call.
        #[cfg(feature = "sim-profile")]
        let _profile_sample = crate::sim::profile::sample_set_val();
        let idx = signal_id.index();
        let unchanged = match self.vals[idx].as_narrow() {
            Some((stored_width, stored_bits)) => {
                stored_width == width && self.vals[idx].typ_id() == typ && stored_bits == bits
            }
            None => self.vals[idx] == Value::from_u64(typ, width, bits),
        };
        if unchanged {
            return false;
        }
        self.vals[idx] = Value::from_u64(typ, width, bits);
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_set_val_write();
        self.mark_changed(signal_id);
        true
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
        self.epoch_clock += 1;
        self.epochs[signal_id.index()] = self.epoch_clock;
        self.dirty.insert(signal_id);
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

    pub(super) fn set_reg(&mut self, signal_id: SignalId, value: Value) {
        self.set_vals[signal_id.index()] = Pending::from_value(value);
    }

    /// The bits-only pending write (PERF-IDEAS item 10): `set_reg`'s
    /// raw twin for narrow drivers.  The caller already knows the
    /// driver's `TypeId`/width, so the pending next-value stores raw
    /// bits without building a `Value`; `transfer` compares raw
    /// integers against the stored bits and tags a `Value` only on an
    /// actual write.
    pub(super) fn set_reg_bits(&mut self, signal_id: SignalId, typ: TypeId, width: Width, bits: u64) {
        self.set_vals[signal_id.index()] = Pending::Bits { typ, width, bits };
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
    /// that claim): `Sim` sets it at registration and rebuilds the
    /// flags from the surviving table after pruning.  `mark_changed`
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sim::bits::BitPat;
    use crate::sim::layout::{TypeInterner, TypeLayoutIndex};
    use crate::types::Type;

    /// Intern `typ` into a fresh layout index (the tag construction
    /// needs, nothing more).
    fn interner_with(typ: &Type) -> (TypeLayoutIndex, TypeId) {
        let mut interner = TypeInterner::new();
        let id = interner.intern(typ);
        (TypeLayoutIndex::from_parts(Default::default(), interner), id)
    }

    /// A hand-built two-signal `State`: signal 0's live value is
    /// `val0`, its pending register next-value is `pending0`, and
    /// signal 1 is a plain (non-register) signal carrying a filler
    /// `Bit` value.  Signal 0 is watched (the write tests assert the
    /// changed-set protocol over it); signal 1 is not, which the
    /// watched-only marking test below leans on.
    fn state_with(val0: Value, pending0: Pending) -> State {
        let (_layouts, bit_typ) = interner_with(&Type::Bit);
        State {
            vals: vec![val0, Value::from_bool(bit_typ, false)],
            set_vals: vec![pending0, Pending::None],
            dirty: DirtyList::new(2),
            watched: vec![true, false],
            changed_since_dispatch: false,
            changed_signals: vec![0; 1],
            changed_count: 0,
            epochs: vec![0, 0],
            epoch_clock: 0,
        }
    }

    /// The bits-only write primitive commits a narrow value only on an
    /// actual raw-bits change: a same-bits write is a full no-op (no
    /// write, no dirty insert, no changed flag) and a changed write
    /// lands the same bits `set_val` would have written.
    #[test]
    fn set_val_bits_commits_narrow_values_only_on_change() {
        let (_layouts, typ) = interner_with(&Type::Word(8));
        let initial = Value::new(typ, BitPat::from_u64(8, 0b1010));
        let mut state = state_with(initial, Pending::None);
        let signal = SignalId::from_index(0);

        // Same bits: no write, no dirty, no changed-since-dispatch.
        state.set_val_bits(signal, typ, 8, 0b1010);
        assert_eq!(state.get(signal).to_u64(), 0b1010);
        assert!(state.pop_dirty().is_none(), "same-bits write must not dirty");
        assert!(!state.changed_since_dispatch());

        // Changed bits: raw compare writes, dirties, raises the flag.
        state.set_val_bits(signal, typ, 8, 0b0101);
        assert_eq!(state.get(signal).to_u64(), 0b0101);
        assert_eq!(state.pop_dirty(), Some(signal));
        assert!(state.changed_since_dispatch());
    }

    /// The raw compare includes the type tag: a write whose tag
    /// differs from the stored value's is a change event, exactly as
    /// the tagged `Value` compare (`set_val`) would see it.  `Bit` and
    /// `Clock` are distinct one-bit types, so the two tags differ at
    /// equal width and bits.
    #[test]
    fn set_val_bits_raw_compare_includes_the_tag() {
        let mut interner = TypeInterner::new();
        let bit_typ = interner.intern(&Type::Bit);
        let clock_typ = interner.intern(&Type::Clock);
        let _layouts = TypeLayoutIndex::from_parts(Default::default(), interner);
        let mut state = state_with(Value::from_bool(bit_typ, true), Pending::None);
        let signal = SignalId::from_index(0);

        // Same bits, different tag: a change (tag mismatch), matching
        // `set_val`'s `Value` equality.
        state.set_val_bits(signal, clock_typ, 1, 1);
        assert_eq!(state.pop_dirty(), Some(signal));
        assert_eq!(state.get(signal).typ_id(), clock_typ);
    }

    /// A wide stored value falls back to the tagged `Value` compare:
    /// the commit still happens on real change and stays silent on an
    /// equal write, exactly as `set_val` behaves.
    #[test]
    fn set_val_bits_falls_back_to_value_compare_for_wide_stored_values() {
        let (_layouts, typ) = interner_with(&Type::Word(100));
        let mut state = state_with(Value::new(typ, BitPat::from_u64(100, 0x1234)), Pending::None);
        let signal = SignalId::from_index(0);

        // Same low bits: the tagged compare says equal, no write.
        state.set_val_bits(signal, typ, 100, 0x1234);
        assert!(state.pop_dirty().is_none());

        // Different low bits: the tagged compare says changed, write.
        state.set_val_bits(signal, typ, 100, 0x4321);
        assert_eq!(state.pop_dirty(), Some(signal));
        assert_eq!(state.get(signal).to_u64(), 0x4321);
    }

    /// Narrow pending bits transfer through the raw compare: the
    /// pending `Value` is only constructed on an actual write, and a
    /// same-bits retransfer is a no-op (the worklist cannot cycle).
    #[test]
    fn transfer_commits_narrow_pending_bits_only_on_change() {
        let (_layouts, typ) = interner_with(&Type::Word(8));
        let mut state = state_with(
            Value::new(typ, BitPat::from_u64(8, 0b1010)),
            Pending::Bits { typ, width: 8, bits: 0b0101 },
        );
        let signal = SignalId::from_index(0);

        state.transfer(signal);
        assert_eq!(state.get(signal).to_u64(), 0b0101);
        assert_eq!(state.pop_dirty(), Some(signal));
        assert!(state.changed_since_dispatch());

        // Retransferring the same pending bits writes nothing.
        state.clear_changed_since_dispatch();
        state.transfer(signal);
        assert!(state.pop_dirty().is_none());
        assert!(!state.changed_since_dispatch());
    }

    /// Wide pending values keep the `Value` representation and
    /// transfer through the old clone-and-`Value`-compare path, with
    /// the same write-only-on-change behavior.
    #[test]
    fn transfer_moves_wide_pending_values_through_the_value_path() {
        let (_layouts, typ) = interner_with(&Type::Word(100));
        let mut state = state_with(
            Value::new(typ, BitPat::from_u64(100, 1)),
            Pending::Value(Value::new(typ, BitPat::from_u64(100, 2))),
        );
        let signal = SignalId::from_index(0);

        state.transfer(signal);
        assert_eq!(state.get(signal).to_u64(), 2);
        assert_eq!(state.pop_dirty(), Some(signal));
        assert!(state.changed_since_dispatch());

        // A same-value retransfer is a no-op.
        state.clear_changed_since_dispatch();
        state.transfer(signal);
        assert!(state.pop_dirty().is_none());
        assert!(!state.changed_since_dispatch());
    }

    /// Watched-only marking: a write to an unwatched signal queues
    /// propagation (the dirty insert is unconditional) but leaves the
    /// changed set untouched — no bit, no count, no sweep-skip gate —
    /// while the same write to a watched signal raises everything.
    #[test]
    fn mark_changed_tracks_only_watched_signals() {
        let (_layouts, typ) = interner_with(&Type::Word(8));
        let mut state = state_with(Value::new(typ, BitPat::from_u64(8, 0b1010)), Pending::None);
        let watched_signal = SignalId::from_index(0);
        let unwatched_signal = SignalId::from_index(1);

        // Unwatched signal: the write lands (propagation still needs
        // it) but the changed set stays untouched.
        state.set_val(unwatched_signal, Value::new(typ, BitPat::from_u64(8, 0b1111)));
        assert_eq!(state.pop_dirty(), Some(unwatched_signal), "the dirty insert is unconditional");
        assert!(!state.changed_since_dispatch(), "unwatched write must not raise the gate");
        assert_eq!(state.changed_signal_count(), 0, "unwatched write must not touch the set");
        assert_eq!(state.changed_signal_ids().count(), 0, "unwatched write must not enter the walk");

        // Watched signal: the same write raises everything.
        state.set_val(watched_signal, Value::new(typ, BitPat::from_u64(8, 0b1111)));
        assert_eq!(state.pop_dirty(), Some(watched_signal));
        assert!(state.changed_since_dispatch());
        assert_eq!(state.changed_signal_count(), 1);
        assert_eq!(state.changed_signal_ids().collect::<Vec<_>>(), vec![watched_signal]);
    }
}
