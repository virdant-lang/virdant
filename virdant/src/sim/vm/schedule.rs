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
}
