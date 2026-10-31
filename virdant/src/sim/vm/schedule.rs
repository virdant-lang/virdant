//! The static combinational schedule (stage A of the scheduled kernel).
//!
//! Built once at VM compile time from the compiled drivers:
//!
//! * each driver's input-signal list: the signals its optimized
//!   bytecode actually `Load`s (the peer alone for an alias copy), a
//!   deduped list of only real data reads;
//! * a topological order over the drivers' combinational dependency
//!   graph.  At stage A this order validates the graph and anchors the
//!   scheduling data model; a later cached straight-line schedule will
//!   consume it directly.
//!
//! At flow time the dynamic worklist loop reads the per-driver input
//! list and the per-signal change-epochs: a driver already evaluated
//! within a settlement whose triggering signal has not been re-written
//! since that evaluation would recommit the output it already
//! committed, so the visit is skipped.  The per-driver input list and
//! the epochs are exactly the data model a cached straight-line
//! schedule needs later.
//!
//! Stage B: the cached straight-line schedule (`ScheduleCache` here).
//! On a settlement whose worklist repeats often (a clock edge settling
//! a chain of registers is the common case), the dynamic pop/gather
//! loop's machinery — a worklist pop, a sensitivity-list walk, and the
//! epoch check per visit — is replaced by one flat list of driver
//! indexes.  The cache is keyed by the settlement's dirty-set signature
//! (the worklist's exact insertion-order sequence, hashed for the slot
//! and compared element-wise for identity — see the soundness comment
//! below), and its value is the *evaluation sequence* the dynamic loop
//! produced for that exact worklist: the drivers' first visits in
//! worklist order.
//!
//! A design whose combinational graph has a cycle gets no schedule:
//! [`TopoSchedule::build`] returns `None`, `Program::schedule` stays
//! `None`, and `flow` keeps the dynamic worklist path, which reaches a
//! fixpoint on any graph.  The dynamic path is the safety net; a
//! schedule is an optimization, never a requirement.

/// The static schedule over one design's drivers.
///
/// Drivers and signals are both identified by `SignalId::index()`
/// (every driver is the component it drives).  `order` and `inputs`
/// cover exactly the components that have a driver expression or an
/// alias peer; everything else is inert for scheduling.
pub(in crate::sim) struct TopoSchedule {
    /// Every driver, in topological order: a driver's position is
    /// strictly after every driver whose output it (transitively)
    /// reads through a propagating edge.  Register drivers (whose
    /// commit writes only the pending next-value, never a live value
    /// mid-flow) and register alias copies have no propagating output,
    /// so they sit wherever their inputs put them and expand nothing.
    order: Vec<u32>,
    /// `pos[component]` is the component's position in `order`, or
    /// `NOT_SCHEDULED` for components that are not drivers.
    pos: Vec<u32>,
    /// `inputs[component]` is the driver's deduped input-signal list
    /// (empty for non-drivers): the signals its optimized bytecode
    /// actually loads, or the alias peer alone.
    inputs: Vec<Box<[u32]>>,
}

/// Sentinel `pos` value for components with no driver.
const NOT_SCHEDULED: u32 = u32::MAX;

impl TopoSchedule {
    /// The driver order (topological; see the field doc).
    pub(in crate::sim) fn order(&self) -> &[u32] {
        &self.order
    }

    /// The component's position in `order`, or `NOT_SCHEDULED` when it
    /// is not a driver.  Read by the debug validators (and the tests);
    /// the flow-time epoch skip works off `inputs` alone.
    pub(in crate::sim) fn pos(&self, component: usize) -> u32 {
        self.pos[component]
    }

    /// Debug validation of the schedule's internal consistency: `order`
    /// is a permutation (every entry in component range, `pos` its
    /// exact inverse), every component with a driver expression is
    /// scheduled, and every input list stays in component range.  The
    /// topological property itself is checked against the circuit at
    /// build time (see `compile_program`).
    pub(in crate::sim) fn validate(&self, n_components: usize, has_expr: &[bool]) {
        assert_eq!(self.pos.len(), n_components, "schedule: pos table size");
        assert_eq!(self.inputs.len(), n_components, "schedule: inputs table size");
        assert!(self.order.len() <= n_components, "schedule: order longer than the component table");
        for (position, &d) in self.order.iter().enumerate() {
            assert!((d as usize) < n_components, "schedule: order entry out of range");
            assert_eq!(self.pos[d as usize], position as u32, "schedule: pos is not order's inverse");
            // Alias sinks are scheduled without a driver expression
            // (their commit is the peer copy), so only the converse
            // direction is asserted below.
        }
        for c in 0..n_components {
            if has_expr[c] {
                assert_ne!(self.pos[c], NOT_SCHEDULED, "schedule: driver {c} not scheduled");
            }
            for &s in &*self.inputs[c] {
                assert!((s as usize) < n_components, "schedule: input out of range");
            }
        }
    }

    /// The driver's deduped input-signal list (empty for non-drivers).
    pub(in crate::sim) fn inputs(&self, component: usize) -> &[u32] {
        &self.inputs[component]
    }

    /// Build the schedule from per-signal driver facts, or `None` when
    /// the combinational graph has a cycle.
    ///
    /// All three tables are densely indexed by `SignalId::index()`:
    ///
    /// * `is_driver[s]`: `s` has a driver expression or an alias peer,
    ///   so `s` is a schedule node (its producer evaluates to it);
    /// * `propagates[s]`: `s` is a driver whose commit writes a live
    ///   value mid-flow (a combinational driver or combinational alias
    ///   copy).  Only these start propagation, so only these carry
    ///   graph edges out;
    /// * `reads[s]`: for a driver, the deduped input-signal list.
    ///
    /// The edge set is then: `s -> d` for every driver `d` with `s` in
    /// `reads[d]` and `propagates[s]`.  A register's live value only
    /// changes through a clock-edge transfer (which re-enters `flow` as
    /// a fresh dirty seed), never through its own driver's evaluation,
    /// so a register is a graph source, never a cycle member.
    pub(in crate::sim) fn build(
        n_signals: usize,
        is_driver: &[bool],
        propagates: &[bool],
        reads: &[Box<[u32]>],
    ) -> Option<TopoSchedule> {
        // Reverse adjacency: for a propagating driver `s`, the drivers
        // that read `s` (in ascending component order, since `reads`
        // is walked in ascending order).  Only propagating signals
        // carry edges out, so only they need a list.
        let mut readers: Vec<Vec<u32>> = vec![Vec::new(); n_signals];
        let mut indegree = vec![0u32; n_signals];
        let mut driver_count = 0usize;
        for d in 0..n_signals {
            if !is_driver[d] {
                continue;
            }
            driver_count += 1;
            for &s in &*reads[d] {
                debug_assert!((s as usize) < n_signals, "schedule: input out of range");
                if !propagates[s as usize] {
                    continue;
                }
                indegree[d] += 1;
                readers[s as usize].push(d as u32);
            }
        }

        // Kahn's algorithm, FIFO seeded in ascending component order
        // (and `readers` ascending), so the produced order is a
        // deterministic function of the graph.
        let mut queue: std::collections::VecDeque<u32> = std::collections::VecDeque::new();
        for d in 0..n_signals {
            if is_driver[d] && indegree[d] == 0 {
                queue.push_back(d as u32);
            }
        }
        let mut order = Vec::with_capacity(driver_count);
        while let Some(d) = queue.pop_front() {
            order.push(d);
            for &r in &readers[d as usize] {
                indegree[r as usize] -= 1;
                if indegree[r as usize] == 0 {
                    queue.push_back(r);
                }
            }
        }
        // Every driver must have been placed; leftovers sit on a cycle.
        if order.len() != driver_count {
            return None;
        }

        let mut pos = vec![NOT_SCHEDULED; n_signals];
        for (position, &d) in order.iter().enumerate() {
            pos[d as usize] = position as u32;
        }
        let inputs = reads.to_vec();
        Some(TopoSchedule { order, pos, inputs })
    }
}

/// Soundness of the cached straight-line schedule.
///
/// The dynamic loop's evaluation sequence for a settlement with worklist W
/// is deterministic given W's insertion order:
///
/// 1.  *The worklist does not grow.*  Every driver in W's induced visit set
///     is "terminal": its commit cannot start propagation (a register driver
///     writes only the pending next-value, which makes no signal dirty; a
///     combinational driver/alias sink whose output has no driver reader and
///     clocks no register makes no signal reachable).  The insertion-time check
///     (`is_terminal`) is proved for every cached worklist, so the loop pops
///     exactly W's signals in W's LIFO order and visits nothing else.
/// 2.  *First visits are never skipped.*  The change-epoch skip (`Sim::
///     eval_and_commit`) fires only when `last_run[d] >= settlement_base`.  At
///     settlement entry every driver's `last_run` predates the settlement's
///     first write epoch (the worklist was populated by writes that bumped
///     `epoch_clock` after the previous settlement's last eval), so the first
///     visit of any driver carries an unprimed `last_run` and always evaluates.
/// 3.  *Repeat visits are no-ops or epoch-skipped.*  A driver appearing on
///     several worklist signals' sensitivity lists is re-visited; with no
///     propagation its inputs cannot have changed since its first evaluation,
///     so the recomputed output equals the committed one (`set_val` is a no-op)
///     and the epoch skip fires exactly as the dynamic loop's.  The cached
///     schedule stores every visit the dynamic loop attempted, as
///     `(trigger, driver)` pairs, so the hit path replays them through the
///     same `eval_and_commit` epoch check the dynamic loop ran; the
///     evaluations (and skips) it replays are exactly the dynamic loop's,
///     in exactly the dynamic loop's order.
///
/// *Signature.*  The key is the worklist's insertion-order sequence, not its
/// sorted set: two settlements with the same signal set but different LIFO
/// orders visit their drivers in different first-visit orders, so a set-only
/// key would collide.  The key is stored in the entry and compared element-wise
/// on every lookup, so a hash collision (two different sequences mapping to the
/// same slot) is detected and treated as a miss — never a silent wrong schedule.

/// The cached straight-line schedule for one `Sim`.
///
/// A direct-mapped cache: `SCHEDULE_CACHE_SLOTS` slots, each either empty or
/// holding one entry (worklist signature + evaluation sequence).  A collision
/// overwrites the victim — the cheapest bounded-size policy; the hit rate is
/// measured per bench, not tuned.  Total memory is bounded by
/// `SLOTS * 2 * max_worklist * 4` bytes.
pub(in crate::sim) struct ScheduleCache {
    slots: Vec<Option<Entry>>,
    /// How many slots are occupied.  Lets a caller skip the lookup's
    /// fixed hash + probe cost while the cache is empty — a design
    /// whose settlements are never terminal never inserts, so its
    /// lookups would be pure overhead forever (measured on the
    /// hello bench: 0 hits over every settlement of the run).
    resident: usize,
    /// Consecutive eligible (in-bounds) misses without a hit.  Counts
    /// only settlements that passed the length bounds — a settlement
    /// too small to ever be cached (s1's falling-edge worklist) must
    /// not advance the guard, or it would disable a cache its neighbor
    /// settlement is actively hitting.
    consecutive_misses: u32,
    /// True once the thrash guard has tripped: the caller skips the
    /// whole cache machinery (stack view, lookup, capture, recording)
    /// and runs the plain dynamic loop, so a design whose settlements
    /// never repeat pays nothing beyond one branch per settlement.
    /// Disabling is only ever cleared by `clear()` — a structural
    /// change is exactly the event that could make the cache pay again.
    disabled: bool,
}

struct Entry {
    /// The worklist's insertion-order signal-index sequence.  Stored
    /// in full and compared on lookup: this is the collision guard
    /// that makes a slot hit exact.
    key: Vec<u32>,
    /// The evaluation sequence: the driver visits the dynamic loop
    /// attempted, as `(trigger_signal_index, driver_index)` pairs in
    /// worklist order.  Every attempt is stored (including repeat
    /// visits): the hit path replays them through the same
    /// `eval_and_commit` epoch check the dynamic loop ran, so a visit
    /// whose trigger epoch predates its last eval is skipped exactly as
    /// the dynamic loop would skip it.
    value: Vec<(u32, u32)>,
}

impl ScheduleCache {
    /// The number of slots.  The 512-stage chain settles with one
    /// worklist sequence per cycle; a handful of slots captures it
    /// while keeping the whole cache under a few hundred KiB.
    /// 256 slots bound the resident entries at 256 * (max_worklist key +
    /// max_worklist value) * 4 bytes ≈ 4 MiB for a 4096-signal worklist
    /// (256 * 2 * 4096 * 4), which is the documented bounded size; the
    /// direct-mapped overwrite is the eviction policy, and the cap is
    /// measured per bench.
    pub(in crate::sim) const SLOTS: usize = 256;

    /// Settlements larger than this are not cached: the key/value
    /// pair's memory and the O(len) lookup compare would dominate the
    /// machinery they save.  The bench chain is 512 stages; this cap
    /// is several times larger.
    pub(in crate::sim) const MAX_SETTLEMENT: usize = 4096;

    /// Settlements smaller than this never touch the cache at all (no
    /// lookup, no insert, no insert-key copy).  The measured floor is
    /// the single-entry worklist: a 1-entry settlement's dynamic loop
    /// is at most one pop plus its gathers, which the replay could
    /// save, but the lookup's hash + slot probe + key compare is fixed
    /// cost the settlement cannot amortize.  On the s1 bench the
    /// falling-edge settlement is exactly this shape (a 1-entry
    /// worklist with an empty gather) and paid the lookup every cycle
    /// for nothing; gating at 2 keeps its behavior byte-identical to
    /// the dynamic loop while the 2-entry rising settlement still
    /// hits.  Measured on the benches, not tuned.
    pub(in crate::sim) const MIN_SETTLEMENT: usize = 2;

    /// Consecutive eligible misses without a single hit before the
    /// cache disables itself.  Measured on the benches: hello's
    /// settlements (never terminal, mostly zero-visit) trip this after
    /// 64 settlements ≈ 32 cycles, after which hello pays the dynamic
    /// loop plus one branch — its ~8 ns/cycle stage-B regression
    /// vanishes.  A design that hits at least once every 63 eligible
    /// misses keeps the cache live, so s1 (rising settlement hits every
    /// cycle) and the 512-stage chain (hits after one warm-up miss)
    /// never trip.  The trade-off: a design whose rare hits come after
    /// longer miss runs loses them — the counter freezes once
    /// disabled, and only a structural change re-enables.  Measured on
    /// the benches, not tuned.
    pub(in crate::sim) const DISABLE_AFTER_MISSES: u32 = 64;

    pub(in crate::sim) fn new() -> ScheduleCache {
        ScheduleCache {
            slots: (0..Self::SLOTS).map(|_| None).collect(),
            resident: 0,
            consecutive_misses: 0,
            disabled: false,
        }
    }

    /// Whether the thrash guard has tripped.  The caller checks this
    /// once per settlement; when true, no cache work happens at all.
    pub(in crate::sim) fn is_disabled(&self) -> bool {
        self.disabled
    }

    /// Record one eligible miss toward the thrash guard.  Called only
    /// for in-bounds settlements that missed — the caller's bounds gate
    /// is what keeps s1's 1-entry settlements from advancing the
    /// counter.
    pub(in crate::sim) fn note_miss(&mut self) {
        self.consecutive_misses += 1;
        if self.consecutive_misses >= Self::DISABLE_AFTER_MISSES {
            self.disabled = true;
        }
    }

    /// Look up the cached evaluation sequence for exactly this
    /// worklist order.  `None` on a miss or a hash collision.
    pub(in crate::sim) fn lookup(&self, stack: &[u32]) -> Option<&[(u32, u32)]> {
        if self.resident == 0 {
            return None;
        }
        if stack.len() < Self::MIN_SETTLEMENT || stack.len() > Self::MAX_SETTLEMENT {
            return None;
        }
        let slot = (hash_stack(stack) as usize) % Self::SLOTS;
        let entry = self.slots[slot].as_ref()?;
        if entry.key == stack {
            Some(&entry.value)
        } else {
            None
        }
    }

    /// Insert (or overwrite, on a slot collision) the evaluation
    /// sequence for this worklist order.
    pub(in crate::sim) fn insert(&mut self, key: Vec<u32>, value: Vec<(u32, u32)>) {
        if key.len() < Self::MIN_SETTLEMENT || key.len() > Self::MAX_SETTLEMENT {
            return;
        }
        let slot = (hash_stack(&key) as usize) % Self::SLOTS;
        if self.slots[slot].is_none() {
            self.resident += 1;
        }
        self.slots[slot] = Some(Entry { key, value });
    }

    /// Drop every entry: called on any structural change (watcher
    /// registration/deregistration, driver set changes, `finish()`),
    /// since the visited-driver set for a given worklist is a function
    /// of the circuit's sensitivity structure.
    pub(in crate::sim) fn clear(&mut self) {
        self.slots.iter_mut().for_each(|slot| *slot = None);
        self.resident = 0;
        // A structural change is also the only event that re-enables a
        // thrash-disabled cache: whatever made the cached sequences
        // stale may have changed the settlement shapes too.
        self.consecutive_misses = 0;
        self.disabled = false;
    }
}

/// FNV-1a over the worklist's signal indexes, in order.
fn hash_stack(stack: &[u32]) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325;
    for &i in stack {
        hash ^= i as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

/// Soundness of the deep-cycle replay (stage C).
///
/// The stage-B cache replays a *single settlement*; this schedule
/// replays a recorded *sequence* of settlements — the recurring
/// settlement pattern of a design's cycle — straight-line: no worklist
/// pops, no signature hashing, no per-settlement cache lookup.  The
/// soundness contract is per settlement and purely structural:
///
/// For a settlement whose every visited driver is terminal (the same
/// `is_terminal` condition the stage-B cache gates on), the dynamic
/// loop's committed outcome is a pure function of the settlement's
/// entry dirty *set*:
///
/// 1. *The visited-driver set is the union of the dirty signals'
///    sensitivity lists.*  Every pop gathers its signal's sensitivity
///    list.  A terminal driver's commit cannot trigger a visit:
///    registers write only the pending next-value (never a live
///    value, never dirty), and a no-fanout output's sensitivity list
///    is empty, so its pop gathers nothing.  No mid-settlement pop
///    gathers anything, so the visited set is exactly
///    `∪ sens(s) for s in S`.
/// 2. *Input values are frozen for the whole settlement.*  The only
///    mid-settlement writes are the visited drivers' own commits, and
///    no visited driver reads another's output (terminal ⟹ no driver
///    reader).  Every evaluation of a driver therefore computes the
///    same value, and the settlement's committed state is
///    order-independent — which is why the replay may run the drivers
///    in its own canonical (topological) order.
/// 3. *Each driver in the union evaluates exactly once in the dynamic
///    loop, and every repeat visit is a provable no-op.*  A driver's
///    first visit always evaluates: its last-eval snapshot predates
///    this settlement's base (the snapshot was taken in an earlier
///    settlement and the write counter is monotonic), so the skip
///    gate's freshness condition fails.  Every repeat visit skips:
///    only entry-dirty signals trigger visits (see 1), so the
///    trigger's write epoch is at most the settlement base, which is
///    at most the driver's this-settlement last-eval snapshot.  Both
///    the evaluated visits and the skipped ones reduce to one
///    commit of `f(entry values)` per driver — the same commit the
///    replay makes.
/// 4. *Therefore* executing each driver of `∪ sens(S)` exactly once,
///    in any order, with no epoch machinery, commits exactly the
///    dynamic loop's outcome — including the no-op reads of
///    constant-folded drivers and the alias-copy commits, which run
///    through the same `eval_and_commit_split` body the dynamic loop
///    uses.  The replay verifies the entry dirty set (exact bitmap
///    equality, plus the length) before executing, so any settlement
///    whose dirty set matches a phase's replays correctly, whatever
///    produced the set: external writes via `set_val`, watcher
///    callbacks, nested `unlock()`s, register transfers all flow
///    through the same per-settlement check.  A settlement whose
///    dirty set does not match runs the unchanged dynamic path.
///
/// The schedule never re-derives `last_run` stamps; after a replay
/// they are stale (older than the dynamic loop would have left them),
/// and stale snapshots can only make a later dynamic settlement's
/// skip gate *fail* (it requires `last_run[d] >= settlement_base`),
/// i.e. evaluate where the dynamic loop would have skipped a provable
/// no-op — correct, merely slower, and only until one dynamic
/// settlement re-stamps the visited drivers.
///
/// *Key.*  The phase's dirty-set bitmap is an exact set identity — not
/// a hash: the check compares the full worklist-width bitmaps
/// element-wise (a memcmp-shaped compare), so two different dirty
/// sets can never compare equal.  (The worklist's insertion
/// *order* is deliberately not part of the key: by (1)-(3) the
/// committed outcome of a terminal settlement is order-independent,
/// so any insertion order with the same set replays identically.)
/// The bitmap is stored at full worklist width, mostly zeros, so the
/// compare needs no trim bookkeeping in the worklist itself; an
/// activation gate refuses phases whose bitmap is wide relative to
/// their dirty count (a sparse design with many signals would pay
/// more in the compare than the machinery it replaces).
///
/// *Detection.*  While the schedule is inactive, every eligible
/// settlement records its dirty-set signature into a bounded history
/// ring.  After each record, the detector looks for the smallest
/// period `P` such that the last `P` signatures repeat the `P` before
/// them (exact, after a hash pre-filter): the pattern's first repeat
/// promotes it to the replay schedule.  Settlements below the
/// engagement floor (`MIN_ENGAGEMENT`) are excluded from the stream:
/// `record` refuses them, so the history ring is recorded-only by
/// construction, and a below-floor settlement is transparent to the
/// phase cursor — it neither matches nor misses nor advances — so the
/// recorded order is the order the phases are consumed in.  (Had a
/// below-floor settlement been recorded, the detected pattern could
/// contain a phase the cursor can never consume: the active path
/// skips below-floor settlements before the match, so the cursor
/// would misalign on every cycle.  Recorded-only is what makes the
/// exclusion sound by construction rather than by measurement.)
/// The promotion is only an
/// optimization decision — every replay is still verified against its
/// phase's exact dirty set, so a wrong or stale detection degrades to
/// the dynamic path, never to a wrong trace.
///
/// *Perturbations.*  Anything that changes a settlement's dirty set
/// (external writes, watcher-driven callbacks, structural changes)
/// breaks the phase check and falls through to the dynamic path.
/// Structural changes (watcher registration, `finish()` pruning) also
/// reset the recorder: the phase driver lists are functions of the
/// circuit's sensitivity structure.  A driver evaluation that drew
/// from the VM's RNG would make evaluation order observable, so
/// phases are refused if any visited driver's bytecode is not a pure
/// function of signal values — the VM ISA has no such instruction
/// (evals are pure), so no check is needed today, and the phase build
/// re-validates terminality from the live circuit at activation.

/// The recorded straight-line program for one settlement of the
/// recurring pattern.
#[derive(Debug)]
pub(in crate::sim) struct Phase {
    /// The settlement's entry dirty-set bitmap at full worklist width
    /// (mostly zero words).  Exact set identity (see the soundness
    /// comment).
    pub(in crate::sim) bits: Box<[u64]>,
    /// The settlement's entry `dirty_len`: the cheap pre-check before
    /// the word compare.
    pub(in crate::sim) len: usize,
    /// The drivers to evaluate, in topological order: the union of the
    /// dirty signals' sensitivity lists — the settlement's eval set
    /// (one evaluation per driver; see the soundness comment).
    pub(in crate::sim) drivers: Box<[u32]>,
}

/// The deep-cycle schedule: a recorded sequence of settlement phases,
/// replayed straight-line while the design's settlement sequence
/// keeps matching it.
#[derive(Debug)]
pub(in crate::sim) struct CycleSchedule {
    /// The recorded phase sequence; the current phase is
    /// `phases[pos]`.
    phases: Vec<Phase>,
    /// Index of the phase the *next* settlement must match.
    pos: usize,
    /// `RECORDING`: every eligible settlement is recorded into the
    /// history ring and the detector runs.  `ACTIVE`: settlements are
    /// matched against `phases[pos]` and replayed on a match.
    /// `OFF`: the recorder gave up (a design whose settlements never
    /// repeat pays nothing); re-enabled only by a structural change.
    state: CycleState,
    /// Settlements handled since the last full phase wrap.  The
    /// active-path thrash guard: a schedule that never completes its
    /// cycle (the design's pattern moved on) deactivates and the
    /// recorder re-arms.
    stale: usize,
    /// The recorder's history ring: the last recorded settlements'
    /// signatures, oldest first.
    history: std::collections::VecDeque<HistoryEntry>,
    /// Settlements seen since the last activation.  The recording
    /// give-up guard: a design whose settlements never form a
    /// repeating pattern stops recording.  Counts every settlement
    /// seen while recording — captured (above the engagement floor) or
    /// below-floor-skipped, which still pays the recording state's
    /// prologue — not only accepted records: a design whose
    /// settlements are all below the floor or all non-replayable must
    /// reach the Off steady state instead of paying the prologue
    /// forever.
    record_streak: usize,
    /// Detections whose phase build failed the terminality validation.
    failures: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::sim) enum CycleState {
    Recording,
    Active,
    Off,
}

/// One recorded settlement signature in the history ring.
#[derive(Debug)]
struct HistoryEntry {
    len: usize,
    /// FNV-1a over the full dirty-set bitmap (plus `len`): the
    /// detector's cheap pre-filter.  A hash match is verified exactly
    /// before any activation, so collisions cost one compare, never a
    /// wrong schedule.
    hash: u64,
    /// The dirty-set bitmap at full worklist width.
    bits: Box<[u64]>,
}

impl CycleSchedule {
    /// The engagement floor: a settlement whose entry dirty-set is
    /// smaller than this never engages the deep-cycle machinery — not
    /// recorded, not replayed — and takes the unchanged dynamic path.
    ///
    /// Measured derivation: on the s=1 bench (settlements of 2 and 1
    /// dirty signals) the machinery's replay bookkeeping — the exact
    /// bitmap check, the phase bookkeeping, the record calls — cost
    /// ~2 ns per replay more than the 1-2 worklist pops it replaced,
    /// a consistent +1.7% regression.  That bookkeeping is roughly
    /// settlement-length independent, while the pops it replaces scale
    /// with the dirty length, so settlements of 3+ dirty signals are
    /// at worst break-even and the chain's 513-dirty rising
    /// settlements win big.  A below-floor settlement keeps the
    /// dynamic path, which is exact stage-B behavior.
    ///
    /// The floor is a performance decision, never a soundness one —
    /// the per-settlement replay check stays exact regardless.  But
    /// excluding settlements from *recording* is what keeps the mixed
    /// stream correct by construction: the history ring and the phase
    /// sequence are indexed over recorded settlements only, and a
    /// below-floor settlement is transparent to the phase cursor in
    /// every state — it neither records, matches, advances, nor
    /// misses — so a pattern detected over the recorded subsequence is
    /// consumed in exactly that order, whatever below-floor
    /// settlements interleave between its phases (the chain's
    /// [513-dirty rising, 1-dirty falling] cycle records as a
    /// period-1 stream of identical rising signatures).
    pub(in crate::sim) const MIN_ENGAGEMENT: usize = 3;

    /// History ring capacity: the detector needs `2 * MAX_PERIOD`
    /// consecutive signatures to confirm a period.
    pub(in crate::sim) const MAX_PERIOD: usize = 16;
    const HIST_CAP: usize = 2 * Self::MAX_PERIOD;

    /// Eligible settlements recorded without an activation before the
    /// recorder stops trying (bounded warm-up cost: a design whose
    /// settlements never repeat pays the recording for at most this
    /// many settlements, then one branch per settlement).
    pub(in crate::sim) const REC_LIMIT: usize = 1024;

    /// A recording settlement the detection refused this many times
    /// (its phases fail the terminality validation) disables the
    /// recorder: re-detecting the same non-replayable pattern every
    /// cycle would pay the union-of-sensitivity-lists build forever.
    const MAX_ACTIVATION_FAILURES: u32 = 4;

    pub(in crate::sim) fn new() -> CycleSchedule {
        CycleSchedule {
            phases: Vec::new(),
            pos: 0,
            state: CycleState::Recording,
            stale: 0,
            history: std::collections::VecDeque::new(),
            record_streak: 0,
            failures: 0,
        }
    }

    /// The deep-cycle machinery's state for this settlement.
    #[inline]
    pub(in crate::sim) fn state(&self) -> CycleState {
        self.state
    }

    /// The phase the next settlement must match, or `None` when it
    /// does not: the caller falls through to the unchanged dynamic
    /// path (the pointer stays put — a settlement that is not part of
    /// the pattern, e.g. a one-off external write, must not consume a
    /// phase slot).  `bits` is the live dirty-set bitmap at full
    /// worklist width, compared element-wise against the phase's.
    pub(in crate::sim) fn match_phase(&self, len: usize, bits: &[u64]) -> Option<&[u32]> {
        debug_assert_eq!(self.state, CycleState::Active);
        let phase = self.phases.get(self.pos)?;
        if phase.len != len || phase.bits.len() != bits.len() {
            return None;
        }
        if phase.bits.as_ref() != bits {
            return None;
        }
        Some(&phase.drivers)
    }

    /// The settlement matched and replayed: advance to the next
    /// phase, wrapping at the end of the pattern (a full cycle) and
    /// resetting the thrash guard.
    pub(in crate::sim) fn advance(&mut self) {
        debug_assert_eq!(self.state, CycleState::Active);
        self.pos += 1;
        if self.pos == self.phases.len() {
            self.pos = 0;
            self.stale = 0;
        }
    }

    /// The active settlement did not match its phase: count it toward
    /// the thrash guard.  Deactivation re-arms the recorder with a
    /// clean history so a changed pattern is re-detected.
    pub(in crate::sim) fn note_miss(&mut self) {
        debug_assert_eq!(self.state, CycleState::Active);
        self.stale += 1;
        if self.stale > self.phases.len() * 8 + 32 {
            self.deactivate();
        }
    }

    fn deactivate(&mut self) {
        self.state = CycleState::Recording;
        self.phases.clear();
        self.pos = 0;
        self.stale = 0;
        self.history.clear();
        self.record_streak = 0;
    }

    /// Snapshot the entry dirty-set bitmap for the recorder (the
    /// capture site calls this once per captured settlement): counts
    /// toward the recording give-up guard, which fires after
    /// `REC_LIMIT` captures without an activation.
    pub(in crate::sim) fn note_capture(&mut self) {
        debug_assert_eq!(self.state, CycleState::Recording);
        self.record_streak += 1;
        if self.record_streak > Self::REC_LIMIT {
            self.state = CycleState::Off;
        }
    }

    /// Record one eligible settlement's signature and run the period
    /// detector.  `Ok(())` when nothing fired; `Err(entries)` returns
    /// the detected pattern's signatures (the last `P` entries) for
    /// the caller to build phases from — the caller's activation can
    /// still fail (non-terminal visited drivers), which the caller
    /// reports through `note_activation_failure`.  A below-floor
    /// settlement is refused and never enters the history ring: the
    /// caller gates them before the capture, so this is defense in
    /// depth, but it is what keeps the history ring recorded-only by
    /// construction (see `MIN_ENGAGEMENT`).
    pub(in crate::sim) fn record(&mut self, len: usize, bits: &[u64]) -> Result<(), Vec<(usize, Vec<u64>)>> {
        debug_assert_eq!(self.state, CycleState::Recording);
        if len < Self::MIN_ENGAGEMENT {
            return Ok(());
        }
        let hash = hash_words(bits, len);
        if self.history.len() == Self::HIST_CAP {
            self.history.pop_front();
        }
        self.history.push_back(HistoryEntry { len, hash, bits: bits.into() });
        // Period detection: the smallest P whose last P signatures
        // repeat the P before them.  The hash filter is exact-safe:
        // equal signatures hash equal, so a true period is never
        // missed; a hash match is verified element-wise before
        // returning.
        for period in 1..=Self::MAX_PERIOD.min(self.history.len() / 2) {
            let n = self.history.len();
            let (recent, earlier) = (
                self.history.range(n - period..).rev(),
                self.history.range(n - 2 * period..n - period).rev(),
            );
            if recent.zip(earlier).all(|(a, b)| {
                a.hash == b.hash && a.len == b.len && a.bits.as_ref() == b.bits.as_ref()
            }) {
                let entries: Vec<(usize, Vec<u64>)> = self
                    .history
                    .range(n - period..)
                    .map(|e| (e.len, e.bits.to_vec()))
                    .collect();
                // Reset the recording streak: a fresh schedule starts a
                // fresh give-up budget.
                self.record_streak = 0;
                return Err(entries);
            }
        }
        Ok(())
    }

    /// The caller built the phases and the schedule is live.  Phases
    /// are built from recorded entries only, so every phase's length
    /// is above the engagement floor — a below-floor settlement can
    /// therefore never match a phase (its length differs from every
    /// phase's), which is what makes the excluded stream replay-safe.
    pub(in crate::sim) fn activate(&mut self, phases: Vec<Phase>) {
        debug_assert_eq!(self.state, CycleState::Recording);
        debug_assert!(
            phases.iter().all(|p| p.len >= Self::MIN_ENGAGEMENT),
            "phases are built from recorded entries, all above the engagement floor",
        );
        self.phases = phases;
        self.pos = 0;
        self.stale = 0;
        self.state = CycleState::Active;
    }

    /// The detected pattern failed the caller's validation (a phase's
    /// visited drivers are not all terminal): count it.  The pattern
    /// cannot ever replay, so repeated detections would only repeat
    /// the failed build; enough failures stop the recorder.
    pub(in crate::sim) fn note_activation_failure(&mut self) {
        self.record_streak = 0;
        self.history.clear();
        self.failures += 1;
        if self.failures >= Self::MAX_ACTIVATION_FAILURES {
            self.state = CycleState::Off;
        }
    }

    /// Structural change (watcher registration, driver set change,
    /// `finish()` pruning): the phase driver lists are functions of
    /// the circuit's sensitivity structure, so everything is dropped.
    pub(in crate::sim) fn reset(&mut self) {
        self.phases = Vec::new();
        self.pos = 0;
        self.stale = 0;
        self.state = CycleState::Recording;
        self.history.clear();
        self.record_streak = 0;
    }
}

/// FNV-1a over the dirty-set bitmap words, salted with the dirty
/// length so differing lengths with word-equal tails hash differently.
fn hash_words(bits: &[u64], len: usize) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325 ^ (len as u64).wrapping_mul(0x100000001b3);
    for &w in bits {
        hash ^= w;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::analysis::elaboration::SignalId;
    fn id(index: usize) -> SignalId {
        SignalId::from_index(index)
    }

    /// Schedule builder over plain indexes: `is_driver` true for every
    /// index in `drivers`, everything propagates unless listed.
    fn build_with(
        n: usize,
        drivers: &[usize],
        reads: &[Box<[u32]>],
        inert: &[usize],
    ) -> Option<TopoSchedule> {
        let mut is_driver = vec![false; n];
        for &d in drivers {
            is_driver[d] = true;
        }
        let mut propagates = is_driver.clone();
        for &i in inert {
            propagates[i] = false;
        }
        TopoSchedule::build(n, &is_driver, &propagates, reads)
    }

    /// The diamond: A feeds B and C, both feed D.  The schedule must
    /// place B and C (in either relative order) strictly before D.
    #[test]
    fn diamond_orders_both_branches_before_the_join() {
        // 0 = A, 1 = B, 2 = C, 3 = D; all four are drivers.
        let reads = vec![
            Box::from([] as [u32; 0]),
            Box::from([0u32]),
            Box::from([0u32]),
            Box::from([1u32, 2]),
        ];
        let schedule = build_with(4, &[0, 1, 2, 3], &reads, &[]).expect("acyclic");
        assert_eq!(schedule.order().len(), 4);
        assert!(schedule.pos(1) < schedule.pos(3), "B before D");
        assert!(schedule.pos(2) < schedule.pos(3), "C before D");
        assert_eq!(schedule.pos(0), 0, "the source A comes first");
        assert_eq!(schedule.inputs(id(3).index()), &[1, 2], "D reads B and C");
    }

    /// A two-node combinational cycle gets no schedule (the dynamic
    /// worklist path stays the fallback), and a driver reading its own
    /// output is caught as the self-edge it is.
    #[test]
    fn cycles_are_rejected() {
        let reads = vec![
            Box::from([] as [u32; 0]),
            Box::from([2u32]),
            Box::from([1u32]),
        ];
        assert!(build_with(3, &[1, 2], &reads, &[]).is_none(), "2 <-> 3 cycle");

        let self_loop = vec![Box::from([] as [u32; 0]), Box::from([1u32])];
        assert!(build_with(2, &[1], &self_loop, &[]).is_none(), "self-edge");
    }

    /// Register drivers are leaves: they read live values (including
    /// their own register) but their commit starts no propagation, so
    /// they carry no out-edges and never make the graph cyclic.
    #[test]
    fn register_drivers_are_inert_leaves() {
        // 0 = a register (inert producer), 1 = its driver (reads 0),
        // 2 = a combinational driver reading the register's live value.
        let reads = vec![
            Box::from([] as [u32; 0]),
            Box::from([0u32]),
            Box::from([0u32]),
        ];
        let schedule = build_with(3, &[0, 1, 2], &reads, &[0]).expect("acyclic");
        assert_eq!(schedule.order().len(), 3);
        // The register's driver has a position (it is scheduled and
        // epoch-skippable), but nothing depends on its placement.
        assert_ne!(schedule.pos(1), NOT_SCHEDULED);
    }

    /// Non-drivers never appear in the schedule at all.
    #[test]
    fn non_drivers_are_not_scheduled() {
        let reads = vec![Box::from([] as [u32; 0]); 2];
        let schedule = build_with(2, &[], &reads, &[]).expect("acyclic");
        assert_eq!(schedule.order().len(), 0);
        assert_eq!(schedule.pos(0), NOT_SCHEDULED);
        assert_eq!(schedule.pos(1), NOT_SCHEDULED);
        assert!(schedule.inputs(0).is_empty());
    }

    /// Cache roundtrip: a stored worklist signature replays exactly the
    /// stored visit sequence.
    #[test]
    fn cache_roundtrips_a_sequence() {
        let mut cache = ScheduleCache::new();
        let stack = vec![7u32, 3, 1];
        let visits = vec![(7u32, 9u32), (3, 9), (1, 10)];
        cache.insert(stack.clone(), visits.clone());
        assert_eq!(cache.lookup(&stack), Some(visits.as_slice()));
    }

    /// The soundness contract's signature claim: two dirty-sets with the
    /// same signal set but different LIFO insertion orders are distinct
    /// keys — inserting one never serves the other.
    #[test]
    fn cache_distinguishes_insertion_orders_of_the_same_set() {
        let mut cache = ScheduleCache::new();
        cache.insert(vec![1u32, 2], vec![(2u32, 9u32), (1, 10)]);
        // Same set {1, 2}, reversed LIFO order: a different settlement
        // with a different visit order — the lookup must miss, not
        // silently replay the first order's schedule.
        assert_eq!(cache.lookup(&[2u32, 1]), None);
        // And the original order still hits.
        assert!(cache.lookup(&[1u32, 2]).is_some());
    }

    /// A hash collision (two different sequences mapping to the same
    /// direct-mapped slot) is detected by the element-wise compare and
    /// treated as a miss — never a silent wrong schedule.
    #[test]
    fn cache_treats_a_hash_collision_as_a_miss() {
        // Find a second two-signal sequence whose FNV-1a slot equals
        // the slot of [1, 2] but whose sequence differs.  Keys below
        // `MIN_SETTLEMENT` are rejected before hashing (see the
        // constant doc), so the collision search uses the same 2-entry
        // shape as a real cacheable settlement.
        let base = hash_stack(&[1u32, 2]) as usize % ScheduleCache::SLOTS;
        let mut colliding = None;
        for k in 2u32..100_000 {
            let candidate = [k, k + 1];
            if candidate == [1u32, 2] {
                continue;
            }
            if hash_stack(&candidate) as usize % ScheduleCache::SLOTS == base {
                colliding = Some(k);
                break;
            }
        }
        let colliding = colliding.expect("a colliding two-signal sequence exists");

        let mut cache = ScheduleCache::new();
        cache.insert(vec![1u32, 2], vec![(1u32, 9u32), (2, 10)]);
        // The colliding key hashes to the same slot but differs
        // element-wise: the lookup must miss.
        assert_eq!(cache.lookup(&[colliding, colliding + 1]), None);
        // And the original key still hits.
        assert!(cache.lookup(&[1u32, 2]).is_some());
    }

    /// Settlements larger than `MAX_SETTLEMENT` are never cached (the
    /// key/value memory and the O(len) compare would dominate the
    /// machinery they save), so an oversized worklist always misses.
    #[test]
    fn cache_rejects_oversized_settlements() {
        let mut cache = ScheduleCache::new();
        let oversized: Vec<u32> = (0..ScheduleCache::MAX_SETTLEMENT as u32 + 1).collect();
        cache.insert(oversized.clone(), vec![(0u32, 0u32)]);
        assert_eq!(cache.lookup(&oversized), None);
    }

    /// The direct-mapped eviction policy: a slot collision overwrites
    /// the victim (the cheapest bounded-size policy; the hit rate is
    /// measured per bench, not tuned).
    #[test]
    fn cache_evicts_on_slot_collision() {
        let mut cache = ScheduleCache::new();
        // Two-entry keys: the `MIN_SETTLEMENT` gate (see its doc)
        // rejects length-1 worklists before hashing, so the eviction
        // test exercises the shape a real settlement takes.
        cache.insert(vec![5u32, 1], vec![(5u32, 9u32), (1, 11)]);
        cache.insert(vec![6u32, 1], vec![(6u32, 10u32), (1, 12)]);
        if hash_stack(&[5u32, 1]) as usize % ScheduleCache::SLOTS
            == hash_stack(&[6u32, 1]) as usize % ScheduleCache::SLOTS
        {
            // Same slot: the second insert evicted the first.
            assert_eq!(cache.lookup(&[5u32, 1]), None);
        }
        // The second key is resident either way.
        assert_eq!(
            cache.lookup(&[6u32, 1]),
            Some([(6u32, 10u32), (1, 12)].as_slice())
        );
    }

    /// Settlements below `MIN_SETTLEMENT` never touch the cache: the
    /// 1-entry falling-edge worklist on the small bench cannot amortize
    /// the lookup's fixed hash + probe cost, so a length-1 key is
    /// rejected before any hashing and never stored or replayed.
    #[test]
    fn cache_rejects_undersized_settlements() {
        let mut cache = ScheduleCache::new();
        cache.insert(vec![9u32], vec![(9u32, 1u32)]);
        assert_eq!(cache.lookup(&[9u32]), None);
        // And it never became resident: an insertion attempt for the
        // same 1-entry worklist also did nothing.
        cache.insert(vec![9u32], vec![(9u32, 2u32)]);
        assert_eq!(cache.lookup(&[9u32]), None);
    }

    // ------------------------------------------------------------------
    // Stage C: the deep-cycle schedule.

    /// Dirty-set bitmap for a set of signal indexes, trimmed to the
    /// highest set word (the `DirtyList::bits`/`top` representation).
    fn cycle_bits(idxs: &[usize]) -> Vec<u64> {
        let top = idxs.iter().max().map(|i| i / 64 + 1).unwrap_or(0);
        let mut bits = vec![0u64; top];
        for &i in idxs {
            bits[i / 64] |= 1 << (i % 64);
        }
        bits
    }

    fn phase_of(bits: &[u64], len: usize, drivers: &[u32]) -> Phase {
        Phase { bits: bits.into(), len, drivers: drivers.into() }
    }

    /// The detector fires on the first exact repetition: a period-2
    /// pattern of two above-floor settlements is detected on its
    /// second repeat.  (The s512 chain's recorded stream is period-1:
    /// its 1-dirty falling edge sits below the engagement floor and is
    /// excluded — see the mixed-stream tests below.)
    #[test]
    fn cycle_detects_a_period_two_pattern_on_its_second_repeat() {
        let mut cs = CycleSchedule::new();
        let first = cycle_bits(&[0, 1, 2]);
        let second = cycle_bits(&[7, 9, 11]);
        assert!(cs.record(3, &first).is_ok());
        assert!(cs.record(3, &second).is_ok());
        // One period is not enough (and a period-1 check correctly
        // rejects: first != second).
        assert!(cs.record(3, &first).is_ok());
        // The second full cycle completes the pattern.
        let entries = cs.record(3, &second).unwrap_err();
        assert_eq!(entries, vec![(3, first.clone()), (3, second.clone())]);
    }

    /// The engagement floor, mixed stream: a recorded/unrecorded
    /// alternating stream.  The history ring is recorded-only by
    /// construction (`record` refuses below-floor lengths), so period
    /// detection runs over the recorded subsequence alone: the two
    /// identical above-floor signatures promote on the second repeat,
    /// with entries from the recorded subsequence only.  (Without the
    /// floor, the interleaved history would promote as a period-2
    /// mixed pattern — [(1, falling), (3, rising)] — whose falling
    /// phase the replay cursor could then never consume: the active
    /// path skips below-floor settlements before the match, so the
    /// cursor would misalign on every cycle.  Recorded-only keeps the
    /// detected pattern consistent with the cursor by construction.)
    #[test]
    fn cycle_floor_excludes_below_floor_settlements_from_detection() {
        let mut cs = CycleSchedule::new();
        let rising = cycle_bits(&[0, 1, 2]);
        let falling = cycle_bits(&[7]);
        assert!(cs.record(1, &falling).is_ok(), "below floor: refused");
        assert!(cs.record(3, &rising).is_ok());
        assert!(cs.record(1, &falling).is_ok(), "below floor: refused");
        // The history holds only the rising signature: the second
        // rising repeats it and the period-1 pattern promotes.
        let entries = cs.record(3, &rising).unwrap_err();
        assert_eq!(entries, vec![(3, rising.clone())]);
    }

    /// Same dirty set, different insertion order: the same bitmap, so
    /// the detection and the replay check are order-blind — which is
    /// the soundness contract (the committed outcome of a terminal
    /// settlement is order-independent).
    #[test]
    fn cycle_signatures_are_set_identities_not_insertion_orders() {
        let mut cs = CycleSchedule::new();
        // Two settlements whose dirty sets are identical (their
        // insertion orders differ, which the bitmap does not record):
        // the period-1 pattern is detected on the second repeat.
        let a = cycle_bits(&[3, 9, 12]);
        assert!(cs.record(3, &a).is_ok());
        let entries = cs.record(3, &a).unwrap_err();
        assert_eq!(entries, vec![(3, a.clone())]);
    }

    /// The replay check is an exact set identity: a matching dirty set
    /// replays its phase's driver list; a different set or length falls
    /// through without consuming the phase pointer.
    #[test]
    fn cycle_match_is_exact_and_mismatches_keep_the_pointer() {
        let mut cs = CycleSchedule::new();
        let rising = cycle_bits(&[0, 1, 2]);
        let falling = cycle_bits(&[7, 9, 11, 13]);
        cs.record(3, &rising).unwrap();
        cs.record(4, &falling).unwrap();
        cs.record(3, &rising).unwrap();
        let entries = cs.record(4, &falling).unwrap_err();
        let phases: Vec<Phase> = entries
            .iter()
            .map(|&(len, ref bits)| match len {
                3 => phase_of(bits, 3, &[10, 11, 12]),
                _ => phase_of(bits, 4, &[]),
            })
            .collect();
        cs.activate(phases);

        assert_eq!(cs.match_phase(3, &rising), Some([10u32, 11, 12].as_slice()));
        cs.advance();
        assert_eq!(cs.match_phase(4, &falling), Some([].as_slice()));
        cs.advance(); // wrapped back to phase 0
        assert_eq!(cs.match_phase(3, &rising), Some([10u32, 11, 12].as_slice()));

        // A different set with the same length rejects.
        assert_eq!(cs.match_phase(3, &cycle_bits(&[0, 1, 3])), None);
        // A different length rejects even with a word-equal bitmap.
        assert_eq!(cs.match_phase(2, &rising), None);
        // Neither mismatch consumed the pointer: the next settlement
        // is still checked against phase 0.
        cs.note_miss();
        cs.note_miss();
        assert_eq!(cs.match_phase(3, &rising), Some([10u32, 11, 12].as_slice()));

        // A below-floor length never matches: no phase can exist below
        // the engagement floor (phases are built only from recorded
        // entries), and the refusal must not consume the pointer.
        assert_eq!(cs.match_phase(1, &cycle_bits(&[5])), None);
        assert_eq!(cs.match_phase(3, &rising), Some([10u32, 11, 12].as_slice()));
    }

    /// The chain's mixed stream, end to end: rising = 513 dirty
    /// signals (above the floor), falling = 1 (below).  With falling
    /// excluded, the recorder sees a stream of identical rising
    /// signatures — period 1.  After activation, the arriving rising
    /// settlement keys to its phase in order; the excluded falling
    /// settlement never matches (no phase can exist below the floor)
    /// and never consumes the phase pointer, so the next rising
    /// settlement still keys to phase 0.
    #[test]
    fn cycle_chain_pattern_replays_across_excluded_settlements() {
        let mut cs = CycleSchedule::new();
        let rising: Vec<usize> = (0..513).collect();
        let rising_bits = cycle_bits(&rising);
        let falling_bits = cycle_bits(&[600]);
        assert!(cs.record(513, &rising_bits).is_ok());
        assert!(cs.record(1, &falling_bits).is_ok(), "below floor: refused");
        let entries = cs.record(513, &rising_bits).unwrap_err();
        assert_eq!(entries, vec![(513, rising_bits.clone())]);

        let drivers: Vec<u32> = (0..513).map(|i| i as u32).collect();
        cs.activate(vec![phase_of(&rising_bits, 513, &drivers)]);

        // The falling edge arrives between rising edges: below the
        // floor it must not match and must not consume the pointer.
        assert_eq!(cs.match_phase(1, &falling_bits), None);
        assert_eq!(cs.match_phase(513, &rising_bits), Some(drivers.as_slice()));
        cs.advance();
        // The next cycle: falling excluded again, rising replays
        // phase 0 once more.
        assert_eq!(cs.match_phase(1, &falling_bits), None);
        assert_eq!(cs.match_phase(513, &rising_bits), Some(drivers.as_slice()));
        cs.advance();
    }

    /// The active-path thrash guard: a schedule that never completes
    /// its cycle deactivates and re-arms the recorder.
    #[test]
    fn cycle_deactivates_when_the_pattern_never_wraps() {
        let mut cs = CycleSchedule::new();
        let a = cycle_bits(&[0, 1, 2]);
        cs.activate(vec![phase_of(&a, 3, &[1])]);
        // phases.len() * 8 + 32 misses are tolerated; one more
        // deactivates.
        for _ in 0..(8 + 32) {
            cs.note_miss();
        }
        assert_eq!(cs.state(), CycleState::Active);
        cs.note_miss();
        assert_eq!(cs.state(), CycleState::Recording);
    }

    /// The recording give-up guard counts captures (not just accepted
    /// records), so a design whose in-bounds settlements are all
    /// non-replayable stops paying the capture after the limit.
    #[test]
    fn cycle_recorder_gives_up_after_the_capture_limit() {
        let mut cs = CycleSchedule::new();
        for i in 0..CycleSchedule::REC_LIMIT {
            cs.note_capture();
            assert_eq!(cs.state(), CycleState::Recording, "capture {i}");
        }
        cs.note_capture();
        assert_eq!(cs.state(), CycleState::Off);
    }

    /// A detected pattern whose phases fail the terminality validation
    /// is reported back; the failure that reaches the limit stops the
    /// recorder (the same pattern would otherwise be re-detected and
    /// re-rejected every cycle, paying the union walk forever).  Each
    /// failure clears the history, so the next round re-detects from
    /// scratch.
    #[test]
    fn cycle_repeated_activation_failures_disable_the_recorder() {
        let mut cs = CycleSchedule::new();
        for round in 1..CycleSchedule::MAX_ACTIVATION_FAILURES {
            assert!(cs.record(3, &a_fixed()).is_ok(), "round {round} first");
            assert!(cs.record(3, &a_fixed()).is_err(), "round {round} detects");
            cs.note_activation_failure();
            assert_eq!(cs.state(), CycleState::Recording, "failure {round}");
        }
        // The failure that reaches the limit disables the recorder.
        assert!(cs.record(3, &a_fixed()).is_ok());
        assert!(cs.record(3, &a_fixed()).is_err());
        cs.note_activation_failure();
        assert_eq!(cs.state(), CycleState::Off);
    }

    fn a_fixed() -> Vec<u64> {
        cycle_bits(&[0, 1, 2])
    }

    /// A structural change re-arms everything: a gave-up recorder
    /// records again, and an active schedule forgets its phases.
    #[test]
    fn cycle_reset_rearms_after_give_up_and_after_activation() {
        let mut cs = CycleSchedule::new();
        for _ in 0..=CycleSchedule::REC_LIMIT {
            cs.note_capture();
        }
        assert_eq!(cs.state(), CycleState::Off);
        cs.reset();
        assert_eq!(cs.state(), CycleState::Recording);

        let a = cycle_bits(&[0, 1, 2]);
        cs.activate(vec![phase_of(&a, 3, &[1])]);
        assert_eq!(cs.state(), CycleState::Active);
        cs.reset();
        assert_eq!(cs.state(), CycleState::Recording);
        assert!(cs.phases.is_empty(), "phases dropped");
    }
}
