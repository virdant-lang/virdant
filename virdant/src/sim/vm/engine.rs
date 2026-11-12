//! The VM engine body: the compiled-bytecode simulator, renamed
//! `VmSim` and stripped of everything the `Sim` facade (`sim::sim`)
//! now owns — the scheduler, the watcher table with its bucketed
//! dispatch, the free-running clocks, simulated time, and the
//! elaboration/callback-registration helpers.  Everything below is
//! value-level: the compiled `Program`, the frame pool, the stage-B
//! settlement schedule cache, the stage-C deep-cycle replay, the
//! change-epoch machinery, and the flow/commit paths.
//!
//! The engine exposes value-level primitives only (`flow`, `flow_all`,
//! `set_without_flow`, `tick_prop`, `get`, `peek`, `circuit`, the
//! changed-set/watched bookkeeping the facade's bucketed dispatch
//! reads, and the `on_structural_change`/`on_clock_attached` hooks).
//! Watcher callbacks are invoked by the facade with `&mut Sim`, so the
//! engine never re-enters dispatch.

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::analysis::symbols::SymbolId;
use crate::common::Width;
use crate::db::Db;
use crate::sim::circuit::Circuit;
use crate::sim::layout::TypeId;
use crate::sim::sim::InitStrategy;
use crate::sim::value::Value;
use super::state::State;

use std::sync::Arc;

use super::{CyclePlanResult, Disqualification, Phase, Program, RunOut, ScheduleCache, CycleSchedule, CycleState, Vm};

pub(crate) struct VmSim {
    circuit: Circuit,
    /// Compiled drivers: one code range per driven component.  Every
    /// driver lowers to bytecode; there is no interpreter fallback.
    program: Program,
    /// The frame pool backing `program`'s drivers.
    vm: Vm,
    state: State,
    /// Per-driver last-eval epoch (see `super::schedule` and
    /// `State::epoch`): `last_run[component]` is the write-counter
    /// snapshot a driver's last evaluation was taken against.  A repeat
    /// visit whose trigger epoch predates it is skipped: the driver
    /// already observed that write, so its committed output is the
    /// value it would recompute.  Zero-initialized, then primed to the
    /// construction-time write count after `flow_all`.
    last_run: Vec<u64>,
    /// Stage B: cached straight-line settlement schedules, keyed by the
    /// settlement's dirty-set signature.  On a cache hit, `flow` executes
    /// the precompiled driver order directly instead of the dynamic
    /// pop/gather/epoch-check loop.  The cache is invalidated on every
    /// structural change (watcher registration/deregistration,
    /// `finish()` pruning — via the facade's `on_structural_change`
    /// hook), because the driver order depends on the circuit's
    /// sensitivity structure.
    schedule_cache: ScheduleCache,
    /// Stage C: the deep-cycle schedule — the recorded settlement
    /// sequence (each phase: dirty-set bitmap + topological driver
    /// list) replayed straight-line while a design's cycle-relevant
    /// settlement sequence repeats exactly.  Soundness and the
    /// replay/abort contract are documented on `super::schedule::
    /// CycleSchedule`; the flow-time integration is at the top of
    /// `flow`.  Placed next to `schedule_cache` so the inactive
    /// per-settlement state load shares the cache line the stage-B
    /// machinery already touches.
    cycle_schedule: CycleSchedule,
    /// The whole-design cycle plan: the qualification verdict over the
    /// circuit + program, computed once at the first `attach_clock`
    /// (via the facade's `on_clock_attached` hook — the primary
    /// free-running clock is only known then).  A qualified design
    /// carries the static schedule data (eval regions, the transfer
    /// point, the combinational tail) that a later phase compiles into
    /// a whole-cycle program; a disqualified design carries the
    /// precise reason.  Nothing behavioral: the engine continues
    /// exactly as today, the plan is stored unused.
    cycle_plan: Option<CyclePlanResult>,
    /// Stage B: per-driver terminality, precomputed once.  A driver is
    /// terminal when its commit cannot start propagation: a register
    /// (its driver writes only the pending next-value, which is never
    /// dirty), or a combinational driver/alias sink whose output signal
    /// has no driver reader and clocks no register.  A settlement whose
    /// every driver is terminal cannot grow its worklist, so the cached
    /// straight-line schedule (which replays the dynamic loop's first
    /// visits) is sound for it.
    is_terminal: Vec<bool>,
    /// Stage B scratch: the settlement's insert key (the worklist's
    /// insertion-order sequence) is copied here on the miss path and
    /// cloned into the cache only when the settlement actually gets
    /// cached — so a settlement that never inserts (a design whose
    /// settlements are never terminal, or a below-`MIN_SETTLEMENT`
    /// worklist) allocates nothing.  The buffer's capacity is reused
    /// across settlements.
    stack_key_scratch: Vec<u32>,
    /// Stage B scratch: the recorded visit sequence.  Same discipline
    /// as `stack_key_scratch`: filled during the miss path's dynamic
    /// loop, moved into the cache on insert, reused across
    /// settlements without reallocation.
    visits_scratch: Vec<(u32, u32)>,
    /// Stage C scratch: the settlement's entry dirty-set bitmap, copied
    /// at `flow` entry while the recorder is capturing (the dynamic
    /// loop consumes the worklist, so the entry set must be snapshotted
    /// before it drains).  Reused across settlements.
    cycle_scratch: Vec<u64>,
    /// Stage C: the settlement's captured entry dirty length, set by
    /// `cycle_settlement` while the recorder is armed and consumed by
    /// the post-settlement record calls (`None`: nothing to record
    /// this settlement).
    cycle_captured: Option<usize>,
    /// The JIT's phase-eligibility table (`jit` feature): one flag per
    /// component, true when the component's type width is at most 64
    /// and its live value therefore always stores the inline `Small`
    /// variant (see `value::Value::jit_layout`).  Only `Load`s of
    /// narrow components can be compiled to direct state reads; a
    /// `Load` of a wide component is heap-backed and refuses the phase.
    /// Kept at the struct's tail (with `cycle_jit`) so the jit feature
    /// does not shift the hot-path fields' offsets: the feature-on
    /// build's inactive cost must stay at the non-jit build's level.
    #[cfg(feature = "jit")]
    signal_narrow: Vec<bool>,
    /// The batch-compiled replay phases (`jit` feature), rebuilt at
    /// every schedule activation.  The compiled functions bake the
    /// state-store and frame pointers, both stable for the engine's
    /// whole lifetime (see `super::jit`'s addressing invariants).  Only
    /// read while the schedule is `Active`: a deactivate clears the
    /// schedule's phases, so a stale list is never used, and the next
    /// activation replaces this wholesale.
    #[cfg(feature = "jit")]
    cycle_jit: Option<super::PhaseJit>,
}

impl VmSim {
    pub(crate) fn new(db: &Db, top: SymbolId, strategy: &InitStrategy) -> VmSim {
        let circuit = Circuit::new(db, top);
        let program = super::compile_program(&circuit);
        let vm_pool = super::Vm::new(&program);

        let n_components = circuit.elaboration.components().len();
        // Stage B terminality: a settlement whose every driver is
        // terminal cannot grow its worklist, so its evaluation sequence
        // is a pure function of the worklist order and is cacheable.
        // See `vm::schedule::ScheduleCache` for the soundness proof.
        let is_terminal: Vec<bool> = (0..n_components)
            .map(|i| {
                let signal_id = SignalId::from_index(i);
                circuit.registers[i]
                    || (circuit.sensitivity_count(signal_id) == 0
                        && circuit.clock_sensitivity_count(signal_id) == 0)
            })
            .collect();
        // The JIT's phase-eligibility table: one flag per component,
        // true when the component's type width is at most 64 and its
        // live value therefore always stores the inline `Small` variant
        // (see `value::Value::jit_layout`).  Only `Load`s of narrow
        // components can be compiled to direct state reads; a `Load` of
        // a wide component is heap-backed and refuses the phase.
        #[cfg(feature = "jit")]
        let signal_narrow: Vec<bool> = (0..n_components)
            .map(|i| {
                let typ = circuit.elaboration.component(SignalId::from_index(i)).typ();
                circuit.type_layout_index.type_width(&typ) <= 64
            })
            .collect();
        let mut engine = VmSim {
            state: State::new(circuit.elaboration.as_ref(), &circuit.type_layout_index, strategy, &circuit.memories),
            circuit,
            program,
            vm: vm_pool,
            last_run: vec![0; n_components],
            schedule_cache: super::ScheduleCache::new(),
            cycle_schedule: super::CycleSchedule::new(),
            cycle_plan: None,
            is_terminal,
            stack_key_scratch: Vec::new(),
            visits_scratch: Vec::new(),
            cycle_scratch: Vec::new(),
            cycle_captured: None,
            #[cfg(feature = "jit")]
            signal_narrow,
            #[cfg(feature = "jit")]
            // The batch-compiled replay phases, rebuilt at every
            // schedule activation (the compiled functions bake the
            // state-store and frame pointers, both stable for the
            // engine's lifetime — see `vm::jit`).  Only read while the
            // schedule is `Active`, so a stale list after a deactivate
            // (which clears the phases) is never used; the next
            // activation replaces it wholesale.
            cycle_jit: None,
        };

        engine.apply_initials();
        engine.flow_all();
        // Prime the last-eval epochs: `flow_all` ran every
        // driver to a fixpoint over the settled values, so each driver's
        // committed output already reflects every write up to now; any
        // later write carries a larger epoch and forces re-evaluation.
        let priming_epoch = engine.state.epoch_clock();
        if let Some(schedule) = engine.program.topo_schedule() {
            for &driver in schedule.order() {
                engine.last_run[driver as usize] = priming_epoch;
            }
        }
        engine
    }

    /// Set every register with an `initial` statement to its initial value
    /// (registers without one keep their `InitStrategy` value).
    /// `initial` expressions are constant, so they are lowered and folded
    /// to a constant value exactly like a driver body, with no referent
    /// bindings (the same bindings an empty eval context produced);
    /// propagation happens in the following `flow_all`.
    fn apply_initials(&mut self) {
        let initials: Vec<(SignalId, Arc<crate::sim::expr::Expr>)> = self
            .circuit
            .initial_exprs
            .iter()
            .map(|(id, expr)| (*id, expr.clone()))
            .collect();
        if initials.is_empty() {
            return;
        }

        for (signal_id, expr) in initials {
            let value = super::compile_initial_value(&self.circuit, &expr);
            self.state.set_val(signal_id, value);
        }
        self.flow();
    }

    pub(crate) fn circuit(&self) -> &Circuit {
        &self.circuit
    }

    pub(crate) fn elaboration(&self) -> Arc<Elaboration> {
        self.circuit.elaboration.clone()
    }

    pub(crate) fn peek(&self, signal_id: SignalId) -> &Value {
        self.state.peek(signal_id)
    }

    /// The per-driver execution seam: every driven component runs
    /// compiled bytecode.  There is exactly one dispatch point, so a
    /// future change to driver execution has one place to land.
    fn eval_driver(&mut self, comp: SignalId) -> Value {
        self.eval_driver_out(comp).into_value()
    }

    /// One driver run shaped for `flow`'s commit paths (PERF-IDEAS
    /// item 10): a narrow (<= 64-bit) VM-compiled result comes back as
    /// raw bits plus the driver's `TypeId`/width, and the bits-only
    /// commit path compares raw integers and tags a `Value` only on an
    /// actual write; everything else (wide results, constant-unit
    /// skips) comes back as the tagged `Value`.
    fn eval_driver_out(&mut self, comp: SignalId) -> RunOut {
        Self::eval_driver_out_split(&self.program, &mut self.vm, &self.state, comp)
    }

    /// `eval_driver_out` over pre-split field borrows: `flow` holds
    /// the circuit as an immutable borrow while committing per driver,
    /// so the per-driver seam must work through disjoint
    /// `program`/`vm`/`state` borrows instead of `&mut self` methods.
    #[inline]
    fn eval_driver_out_split(
        program: &Program,
        vm: &mut Vm,
        state: &State,
        comp: SignalId,
    ) -> RunOut {
        // Constant-unit skip (synthesis graft 2): a driver the
        // optimizer folded entirely to a constant commits that value
        // directly, `flow`/`flow_all` skip the run.  Constants stay on
        // the `Value` path: the folded value is already built.
        if let Some(value) = program.const_value(comp).cloned() {
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_const_skip();
            return RunOut::Tagged(value);
        }
        // Sampled timing, not per-call `Instant`: a read costs ~20 ns,
        // more than some drivers, so only every 256th eval is timed
        // (see `sim::profile`).  The tick also counts the eval.
        #[cfg(feature = "sim-profile")]
        let _profile_sample = crate::sim::profile::sample_eval();
        if program.has_driver(comp) {
            vm.run_out(program, state, comp)
        } else {
            panic!("eval_driver_out: component {comp:?} has no compiled driver");
        }
    }

    /// Evaluate every driven component, repeating until the
    /// combinational network reaches a fixpoint.
    ///
    /// Used at construction: there are no X values, so a component whose
    /// random-initialized bits happen to equal its computed value would
    /// never look "changed" and its dependents would keep stale random
    /// values.  A full-network pass makes every value consistent with
    /// the (initial) register values before the run starts.
    pub(crate) fn flow_all(&mut self) {
        loop {
            let mut changed = false;

            let ids: Vec<SignalId> = self
                .circuit
                .elaboration
                .components()
                .iter()
                .map(|c| c.id())
                .collect();
            for signal_id in ids {
                // Alias-driven components (bare-reference drivers and
                // socket connections) have no expr of their own: their
                // value is a copy of the peer's.  A register in this
                // position must still get its pending staged here —
                // otherwise it would carry the random `InitStrategy`
                // pending into the first clock edge (the dirty-driven
                // `flow` only restages it when the peer's value changes,
                // which may not happen before the first edge).  Master's
                // `flow_all` semantics; the branch's version skipped the
                // staging entirely.
                if let Some(peer_id) = self.circuit.aliases[signal_id.index()] {
                    if self.circuit.registers[signal_id.index()] {
                        self.state.set_reg(signal_id, self.state.get(peer_id));
                    }
                }
                if self.circuit.exprs[signal_id.index()].is_none() {
                    continue;
                }
                let new_value = self.eval_driver(signal_id);
                if self.circuit.registers[signal_id.index()] {
                    // A register's driver computes its *next* value; the
                    // current value only changes on a clock edge.
                    self.state.set_reg(signal_id, new_value);
                } else if self.state.get(signal_id) != new_value {
                    self.state.set_val(signal_id, new_value);
                    changed = true;
                }
            }

            // Socket-connection aliases copy their peer's value.
            let alias_pairs: Vec<(SignalId, SignalId)> = self
                .circuit
                .elaboration
                .components()
                .iter()
                .filter_map(|c| {
                    self.circuit.aliases[c.id().index()]
                        .map(|source| (c.id(), source))
                })
                .collect();
            for (sink, source) in alias_pairs {
                let value = self.state.get(source);
                if self.state.get(sink) != value {
                    self.state.set_val(sink, value);
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }
        self.state.clear_dirty();
        // Construction's full-network pass has observed every value,
        // and no watcher exists yet (watchers registered later capture
        // the live value at registration time), so the first
        // settlement's sweep-skip predicate starts clean.
        self.state.clear_changed_since_dispatch();
        // End of this flow path: the next settlement starts with a
        // clean per-settlement commit-marker set (see `State::
        // clear_committed`).
        self.state.clear_committed();
    }

    /// One clock-edge transfer of `signal_id`'s dependents, settling in
    /// between: the facade's `Sim::tick` drives this through
    /// `tick_prop` plus one final `flow`.  `tick_prop` recurses through
    /// here for nested sensitivity entries, exactly as the legacy
    /// engine's `tick_inner` does.
    fn tick(&mut self, signal_id: SignalId) {
        self.tick_prop(signal_id);
        self.flow();
    }

    pub(crate) fn tick_prop(&mut self, signal_id: SignalId) {
        // Iterate by index against the immutable Circuit so we can recurse
        // mutably into `self.tick` / `self.transfer` without cloning the
        // sensitivity IndexSet on every step.
        let n = self.circuit.sensitivity_count(signal_id);
        for i in 0..n {
            let inner = self.circuit.sensitivity_at(signal_id, i);
            self.tick(inner);
        }

        let m = Self::commit_domain_split(&self.circuit, &mut self.state, signal_id);
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_transfers(m as u64);
        #[cfg(not(feature = "sim-profile"))]
        let _ = m;
    }

    pub(crate) fn flow(&mut self) {
        // Phase-granularity timing only: one `Instant` pair per
        // settlement, never per driver (see `sim::profile`).
        #[cfg(feature = "sim-profile")]
        let _profile_phase = crate::sim::profile::time_flow();
        // The write counter at settlement start: the base the change-
        // epoch skip's already-evaluated gate compares against (see
        // `eval_and_commit`).  The skip runs only when the settlement
        // enters `flow` with a worklist large enough for repeat driver
        // visits to be possible (see `EPOCH_CHECK_MIN_DIRTY`); smaller
        // settlements keep the plain loop.
        let epoch_checks = self.state.dirty_len() >= EPOCH_CHECK_MIN_DIRTY
            && self.program.topo_schedule().is_some();
        let settlement_base = self.state.epoch_clock();

        // Stage C: the deep-cycle replay.  While a recorded settlement
        // sequence (the design's recurring cycle) keeps matching, each
        // settlement is checked against its phase's exact dirty-set
        // bitmap — no worklist pops, no signature hashing, no per-
        // settlement cache lookup — and executed as straight-line code:
        // the phase's drivers (the union of the dirty signals'
        // sensitivity lists) evaluate once each with no epoch
        // machinery.  See `vm::schedule::CycleSchedule` for the
        // soundness proof and the replay/abort contract: a mismatch
        // falls through to the unchanged stage-B/dynamic path below
        // (the phase pointer stays put), and the machinery records
        // instead while a pattern is still being detected.  The
        // recorder's give-up guard bounds the inactive cost: a design
        // whose settlements never form a repeating pattern records for
        // a bounded warm-up, then pays one state load per settlement.
        // The engagement floor keeps trivially small settlements out
        // of the machinery in every state: below
        // `super::CycleSchedule::MIN_ENGAGEMENT` dirty signals the replay
        // bookkeeping costs more than the 1-2 pops it would replace
        // (measured +1.7% on the s=1 bench), so such a settlement is
        // neither recorded nor replayed and the dynamic path below is
        // unchanged stage-B behavior for it.
        //
        // The machinery lives in `cycle_settlement`, not here: the
        // inline footprint of this prologue is the hot path's entire
        // inactive cost (measured: an inline replay loop cost hello
        // ~10% with the schedule never engaging, purely from code
        // layout), so `flow` keeps one load and one predicted branch
        // and the helpers keep the rest out of line.
        let cycle_armed = self.cycle_schedule.state() != CycleState::Off;
        if cycle_armed && self.cycle_settlement() {
            // End of this flow path: the replay committed its transfers
            // through `State::transfer`, whose per-settlement commit
            // markers must not survive into the next settlement.
            self.state.clear_committed();
            return;
        }

        // Stage B: cached straight-line settlement schedule.  The cache
        // key is the worklist's insertion-order sequence.  On a hit, the
        // stored visit sequence is replayed through the same
        // eval/commit machinery the dynamic loop used — including the
        // epoch check, so a repeat visit is skipped exactly as the
        // dynamic loop would skip it — and the pop/gather machinery is
        // skipped entirely.  The lookup borrows the live worklist view
        // (no key copy), and the replay iterates the stored sequence
        // through the cache slice — `eval_and_commit` takes disjoint
        // `&Sim`-field borrows, so no visit-list copy is needed either.
        // The only allocation is on the miss path, which copies the
        // stack once as the insert key (the dynamic loop consumes the
        // worklist it keys off).
        // Thrash guard: a design whose settlements never repeat would
        // pay the fixed per-settlement machinery (stack view, lookup,
        // capture, recording branches) every settlement for nothing —
        // measured at ~8 ns/cycle on hello.  After
        // `DISABLE_AFTER_MISSES` consecutive eligible misses the cache
        // disables itself and the flow below degrades to exactly the
        // stage-A dynamic loop; a structural change (which clears the
        // cache anyway) is the only re-enable.  Disabling is purely a
        // performance decision: the plain dynamic loop is always
        // correct.
        let cache_engaged = !self.schedule_cache.is_disabled();
        let mut cachable = false;
        if cache_engaged {
        // The bounds gate sits before the lookup: a settlement too
        // small (or too large) to ever be cached pays no lookup at all
        // — the s1 bench's 1-entry falling-edge worklist was paying the
        // lookup's hash + probe every cycle for a result it could never
        // use.
        if super::ScheduleCache::MIN_SETTLEMENT <= self.state.dirty_len()
            && self.state.dirty_len() <= super::ScheduleCache::MAX_SETTLEMENT
        {
        let stack_view = self.state.dirty_stack();
        let cached_visits = self.schedule_cache.lookup(stack_view);
        if let Some(visits) = cached_visits {
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_schedule_cache_hit();
            // The replay cannot push a signal whose visit the dynamic
            // loop would have gathered (the cache only stores terminal
            // settlements: a terminal driver's commit can dirty a
            // no-fanout signal, but that signal's sensitivity list is
            // empty, so its pop gathers nothing).  Wiping the worklist
            // after the replay therefore leaves exactly the dynamic
            // loop's end state: an empty worklist with every signal's
            // propagation done by the replayed visits.  The bulk clear
            // replaces the dynamic loop's per-pop machinery.
            for &(trigger_index, driver_index) in visits {
                let trigger = SignalId::from_index(trigger_index as usize);
                let comp = SignalId::from_index(driver_index as usize);
                if epoch_checks {
                    Self::eval_and_commit(
                        &self.circuit,
                        &self.program,
                        &mut self.vm,
                        &mut self.state,
                        &mut self.last_run,
                        settlement_base,
                        trigger,
                        comp,
                    );
                } else {
                    Self::eval_and_commit_split(
                        &self.circuit,
                        &self.program,
                        &mut self.vm,
                        &mut self.state,
                        comp,
                    );
                }
            }
            self.state.clear_dirty();
            // End of this flow path: clear the per-settlement commit
            // markers before returning (the replayed transfers must not
            // survive into the next settlement).
            self.state.clear_committed();
            // The stage-B hit is a replayable settlement (its entry was
            // cached only from a terminal settlement), so the recorder
            // takes it into the history ring unconditionally.  Gated on
            // the same arming check the prologue made: when the recorder
            // is off no capture can exist (it is only taken while the
            // recorder is armed), so the tail load is pure waste.
            if cycle_armed {
                if let Some(len) = self.cycle_captured.take() {
                    self.record_cycle_settlement(len, true);
                }
            }
            return;
        }
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_schedule_cache_miss();
        // Count the miss toward the thrash guard.  Only in-bounds
        // settlements count: a 1-entry settlement never engages the
        // cache, so it must not disable a cache a neighboring
        // settlement is hitting (s1).
        self.schedule_cache.note_miss();

        // Miss: run the dynamic loop, recording the visit sequence for
        // the cache as it goes.  The cached schedule is only sound when
        // the settlement's visit sequence is a pure function of the
        // worklist order — i.e. when every visited driver is terminal
        // (a register, whose commit writes only the pending next-value,
        // or a driver whose output has no fanout, whose commit cannot
        // trigger further visits).  A settlement containing any
        // non-terminal driver can grow its worklist in a data-dependent
        // way, so replaying a recorded sequence for a future settlement
        // with the same signature could differ from the dynamic loop.
        // See `vm::schedule::ScheduleCache` for the soundness proof.
        // The insert key is the entry worklist's insertion-order
        // sequence: the dynamic loop below consumes the worklist it keys
        // off, so the sequence is captured here, on the miss path only,
        // into a reused scratch buffer — no allocation after the first
        // settlement of each length, because the cache's `insert` clones
        // the key, and only when the settlement actually gets cached.
        // The capture itself is gated to the cache's length bounds, so
        // a settlement that can never be cached (a 1-entry falling-edge
        // worklist) does not even pay the memcpy.
        self.stack_key_scratch.clear();
        self.stack_key_scratch.extend_from_slice(stack_view);
        // Recording is live only while the settlement can still be
        // cached: the first non-terminal visit both stops the recording
        // and drops the terminality test for the rest of the settlement,
        // so a design whose settlements are never terminal (hello's
        // combinational tails) pays at most a prefix's worth of
        // bookkeeping per settlement.
        // The scratch is cleared unconditionally at recording start: a
        // settlement that recorded a visit prefix and then hit a
        // non-terminal visit must not leak that prefix into the NEXT
        // settlement's insert — the stored sequence would replay a
        // visit that never belonged to it (a soundness bug the benches
        // never caught, since none of the seven check.sh designs mix a
        // dead-with-prefix settlement next to a cachable one).
        self.visits_scratch.clear();
        cachable = true;
        }
        }
        while let Some(signal_id) = self.state.pop_dirty() {
            let n = self.circuit.sensitivity_count(signal_id);
            for i in 0..n {
                let update_signal_id = self.circuit.sensitivity_at(signal_id, i);
                if cachable {
                    let driver = update_signal_id.index();
                    if self.is_terminal[driver] {
                        self.visits_scratch
                            .push((signal_id.index() as u32, driver as u32));
                    } else {
                        cachable = false;
                    }
                }
                // The branch is loop-invariant, so the gated-off path
                // is the plain one-call commit with no epoch machinery
                // in sight.
                if epoch_checks {
                    Self::eval_and_commit(
                        &self.circuit,
                        &self.program,
                        &mut self.vm,
                        &mut self.state,
                        &mut self.last_run,
                        settlement_base,
                        signal_id,
                        update_signal_id,
                    );
                } else {
                    Self::eval_and_commit_split(
                        &self.circuit,
                        &self.program,
                        &mut self.vm,
                        &mut self.state,
                        update_signal_id,
                    );
                }
            }
        }
        if cachable && !self.visits_scratch.is_empty() {
            // Only cache terminal settlements that actually evaluated
            // something: a settlement with no visits replays to nothing,
            // so caching it only evicts a real schedule through a slot
            // collision (falling-edge worklists are exactly that).
            // Sort the stored visits by the driver's static topological
            // position (see `vm::schedule::ScheduleCache` for why the
            // order is unobservable within a terminal settlement).
            if let Some(schedule) = self.program.topo_schedule() {
                self.visits_scratch
                    .sort_by_key(|&(_, driver)| schedule.pos(driver as usize));
                // The two allocations a cacheable settlement ever pays:
                // the key clone and the visits move into the cache.  Both
                // happen only here, once per inserted schedule; the hit
                // path and the recurring miss path allocate nothing.
                let key = self.stack_key_scratch.clone();
                let value = std::mem::take(&mut self.visits_scratch);
                self.schedule_cache.insert(key, value);
            }
        }
        // Gated on the prologue's arming check (see the hit-path tail):
        // when the recorder is off, no capture exists, so this is a
        // plain register test instead of an Option load from memory.
        if cycle_armed {
            if let Some(len) = self.cycle_captured.take() {
                // `cachable` is the replayability verdict for in-bounds
                // settlements (every visited driver terminal).
                self.record_cycle_settlement(len, cachable);
            }
        }
        // End of this flow path (the dynamic tail): the next settlement
        // starts with a clean per-settlement commit-marker set.
        self.state.clear_committed();
    }

    /// Stage C's per-settlement work, kept out of `flow`'s inline
    /// footprint (see the call site's comment): replay the expected
    /// phase when the entry dirty set matches it, or snapshot the
    /// entry dirty set while the recorder is armed.  Settlements below
    /// the engagement floor (`super::CycleSchedule::MIN_ENGAGEMENT`) are
    /// excluded from the machinery in every state: the dynamic path
    /// below is unchanged stage-B behavior for them, and they are
    /// transparent to the pattern cursor — no capture, no match, no
    /// advance, and no miss (see `vm::schedule::CycleSchedule`).
    #[inline(never)]
    fn cycle_settlement(&mut self) -> bool {
        match self.cycle_schedule.state() {
            CycleState::Active => {
                let len = self.state.dirty_len();
                if len < super::CycleSchedule::MIN_ENGAGEMENT {
                    // Below the engagement floor: the unchanged dynamic
                    // path.  No match attempt — and no miss: an excluded
                    // settlement must not count toward the thrash guard
                    // (the chain's 1-dirty falling edge arrives between
                    // rising-edge replays every cycle and must not drain
                    // the schedule's tolerance).
                    return false;
                }
                let matched_pos = {
                    let bits = self.state.dirty_bits();
                    self.cycle_schedule.match_phase(len, bits).map(|(pos, _)| pos)
                };
                if let Some(pos) = matched_pos {
                    #[cfg(feature = "sim-profile")]
                    crate::sim::profile::bump_cycle_replay();
                    // JIT v2: a batch-compiled phase evaluates all of
                    // its drivers in ONE native call (see
                    // `jit_phase_replay`); phases that did not compile
                    // keep the interpreter loop below.  The split is
                    // per phase, never per driver.  Kept out of line so
                    // this function's machine code stays at the non-jit
                    // build's size.
                    #[cfg(feature = "jit")]
                    if Self::jit_phase_replay(
                        &self.circuit,
                        &self.program,
                        &mut self.state,
                        &mut self.cycle_schedule,
                        self.cycle_jit.as_mut().unwrap(),
                        pos,
                    ) {
                        return true;
                    }
                    #[cfg(all(feature = "jit", feature = "sim-profile"))]
                    crate::sim::profile::bump_jit_phase_fallback();
                    for &d in self.cycle_schedule.phase_drivers(pos) {
                        Self::eval_and_commit_split(
                            &self.circuit,
                            &self.program,
                            &mut self.vm,
                            &mut self.state,
                            SignalId::from_index(d as usize),
                        );
                    }
                    self.state.clear_dirty();
                    self.cycle_schedule.advance();
                    return true;
                }
                #[cfg(feature = "sim-profile")]
                crate::sim::profile::bump_cycle_abort();
                self.cycle_schedule.note_miss();
                false
            }
            CycleState::Recording => {
                let len = self.state.dirty_len();
                if len < super::CycleSchedule::MIN_ENGAGEMENT {
                    // Below the engagement floor: nothing is captured
                    // or recorded — the dynamic path below is exact
                    // stage-B behavior for this settlement, and it is
                    // transparent to the pattern cursor.  It still
                    // counts toward the give-up guard: an all-tiny
                    // design must reach the recorder-off steady state
                    // instead of paying this prologue forever.
                    self.cycle_schedule.note_capture();
                    return false;
                }
                // Snapshot the entry dirty set for the recorder: the
                // dynamic loop below consumes the worklist, so the set
                // must be captured before it drains.  The capture is
                // the recorder's only steady per-settlement cost (one
                // worklist-width bitmap copy) and stops entirely once
                // the give-up guard fires.
                self.cycle_scratch.clear();
                self.cycle_scratch.extend_from_slice(self.state.dirty_bits());
                self.cycle_schedule.note_capture();
                self.cycle_captured = Some(len);
                false
            }
            CycleState::Off => false,
        }
    }
    /// Feed one completed settlement into the deep-cycle recorder and
    /// run its period detector.  `replayable` is the settlement's
    /// terminality verdict (a stage-B hit is replayable by
    /// construction; the dynamic path's `cachable` flag is the
    /// verdict otherwise).  A detected period builds the phases
    /// (union of sensitivity lists per phase, terminality-validated)
    /// and activates the replay schedule.
    #[inline(never)]
    fn record_cycle_settlement(&mut self, entry_len: usize, replayable: bool) {
        // The give-up guard may have fired at the capture site (this
        // settlement's snapshot tripped REC_LIMIT): the recorder is off
        // and nothing is recorded.
        if self.cycle_schedule.state() != CycleState::Recording {
            return;
        }
        // Settlements with a non-terminal visit can never be replayed
        // (their worklist grows data-dependently); recording them would
        // only teach the detector an unusable pattern.  Below-floor
        // settlements never reach this function at all: the capture
        // site skips them, so `cycle_captured` — and with it this call
        // — only exists for above-floor settlements.
        if !replayable {
            return;
        }
        match self.cycle_schedule.record(entry_len, &self.cycle_scratch) {
            Ok(()) => {}
            Err(entries) => match self.build_cycle_phases(&entries) {
                Some(phases) => {
                    #[cfg(feature = "sim-profile")]
                    crate::sim::profile::bump_cycle_activation();
                    // JIT v2: compile the activated phases now, before
                    // installing them — one native function per
                    // qualifying phase; non-qualifying phases keep the
                    // interpreter replay.  The compiled functions bake
                    // engine-owned pointers that are stable for the
                    // engine's whole lifetime (see `vm::jit`'s
                    // addressing invariants), and are only reachable
                    // while the schedule is `Active`, so replacing this
                    // at the next activation is sound.
                    #[cfg(feature = "jit")]
                    {
                        // The narrow-store addressing constants (the
                        // stride between live values and the offset of
                        // the `Small` variant's bits): probed once per
                        // activation against any component's type (the
                        // layout does not depend on which type names
                        // the probe value).  See `Value::jit_layout`.
                        let (stride, bits_off) = crate::sim::value::Value::jit_layout(
                            self.component_type_id(SignalId::from_index(0)),
                        );
                        self.cycle_jit = Some(super::build_phases(
                            &self.program,
                            &phases,
                            &mut self.vm,
                            self.state.jit_vals_base(),
                            stride,
                            bits_off,
                            &self.signal_narrow,
                        ));
                    }
                    self.cycle_schedule.activate(phases);
                }
                None => self.cycle_schedule.note_activation_failure(),
            },
        }
    }
    /// Build the deep-cycle phases from the recorder's detected
    /// pattern, or `None` when the pattern is not replayable.
    ///
    /// Each phase's driver list is the union of its dirty signals'
    /// sensitivity lists — the settlement's eval set (see the
    /// soundness comment on `vm::schedule::CycleSchedule`).  Every
    /// driver must be terminal: a non-terminal driver's commit can
    /// start propagation, which grows the worklist in a way the
    /// straight-line replay could not reproduce.  The order is
    /// topological when the design has a static schedule, ascending
    /// component index otherwise; the order is unobservable (inputs
    /// are frozen for the whole settlement), so both are sound.
    #[inline(never)]
    fn build_cycle_phases(&self, entries: &[(usize, Vec<u64>)]) -> Option<Vec<Phase>> {
        let n = self.circuit.elaboration.components().len();
        let mut marked: Vec<bool> = vec![false; n];
        let mut phases = Vec::with_capacity(entries.len());
        for (len, bits) in entries {
            // Activation gate: the phase check compares the full
            // worklist-width bitmap, so a phase whose bitmap is wide
            // relative to its dirty count would cost more per replayed
            // settlement than the machinery it replaces.  Such a design
            // (sparse activity over a huge component space) keeps the
            // dynamic path.
            if bits.len() > 4 * len + 16 {
                return None;
            }
            let mut drivers: Vec<u32> = Vec::new();
            for (w, &word) in bits.iter().enumerate() {
                let mut word = word;
                while word != 0 {
                    let bit = word.trailing_zeros();
                    word &= word - 1;
                    let signal = SignalId::from_index(w * 64 + bit as usize);
                    let count = self.circuit.sensitivity_count(signal);
                    for i in 0..count {
                        let d = self.circuit.sensitivity_at(signal, i).index();
                        if !marked[d] {
                            marked[d] = true;
                            drivers.push(d as u32);
                        }
                    }
                }
            }
            if drivers.iter().any(|&d| !self.is_terminal[d as usize]) {
                return None;
            }
            if let Some(schedule) = self.program.topo_schedule() {
                drivers.sort_by_key(|&d| schedule.pos(d as usize));
            } else {
                drivers.sort_unstable();
            }
            for &d in &drivers {
                marked[d as usize] = false;
            }
            phases.push(Phase {
                bits: bits.clone().into_boxed_slice(),
                len: *len,
                drivers: drivers.into_boxed_slice(),
            });
        }
        Some(phases)
    }

    /// Evaluate one driven component and commit the result: the shared
    /// per-driver body of the flow loop.
    ///
    /// Aliases (socket connections `:=:`, and bare-reference drivers
    /// resolved by the elaboration): the value is a copy of the peer's.
    /// Edge detection below applies to alias copies too (e.g. a clock
    /// routed through a bare reference or a socket connection).  Alias
    /// copies stay on the `Value` commit path: the peer's `Value` is
    /// already built, and alias copies are not the per-driver hot path
    /// the bits-only path targets.
    ///
    /// On a scheduled design (see `vm::schedule`), and when the caller
    /// enabled the check for this settlement (`epoch_checks`: the
    /// worklist was large enough at entry for repeat visits to be
    /// possible - see `EPOCH_CHECK_MIN_DIRTY`), the change-epoch skip
    /// runs first: a driver already evaluated within this settlement
    /// whose triggering signal has not been re-written since that
    /// evaluation would recommit the output it already committed, so
    /// the visit is dropped.  The skipped eval is a provable no-op,
    /// not a heuristic: every actual write to one of the driver's real
    /// inputs pops that input and re-triggers the driver, so quiescence
    /// still implies every driver's stored value equals its function of
    /// the settled inputs.  With the check disabled the visit is the
    /// plain pre-schedule behavior, bit for bit.
    ///
    /// Returns whether the component's live value was actually written
    /// (register pending-writes, epoch skips, and no-change commits do
    /// not count).
    fn eval_and_commit(
        circuit: &Circuit,
        program: &Program,
        vm: &mut Vm,
        state: &mut State,
        last_run: &mut [u64],
        settlement_base: u64,
        trigger: SignalId,
        comp: SignalId,
    ) -> bool {
        let d = comp.index();
        if let Some(schedule) = program.topo_schedule() {
            // Change-epoch skip.  `settlement_base` (the write counter
            // at this settlement's start) gates the check to drivers
            // already evaluated within this settlement: their last-run
            // snapshot postdates every pre-settlement write, so the
            // only write a re-evaluation could newly observe is one
            // that happened after that snapshot, and the trigger's
            // epoch says whether this visit carries one.  A driver with
            // exactly one bytecode input is excluded from the check
            // entirely: it is triggered only by that signal, so any
            // repeat visit implies the signal was re-written after the
            // last evaluation (fresh epoch) and the check could never
            // skip; dropping it keeps the common single-reader visit at
            // one extra load and compare.
            if last_run[d] >= settlement_base
                && schedule.inputs(d).len() != 1
                && state.epoch_at(trigger.index()) <= last_run[d]
            {
                #[cfg(feature = "sim-profile")]
                crate::sim::profile::bump_epoch_skip();
                return false;
            }
            // Snapshot the write counter before the run: the driver's
            // own commit (a rising clock edge transfers registers,
            // possibly signals this same driver reads) must not mark
            // itself up-to-date against them.
            let epoch_snapshot = state.epoch_clock();
            let wrote = Self::eval_and_commit_split(circuit, program, vm, state, comp);
            last_run[d] = epoch_snapshot;
            wrote
        } else {
            Self::eval_and_commit_split(circuit, program, vm, state, comp)
        }
    }
    /// JIT v2's batch replay (`jit` feature): run the phase's
    /// batch-compiled native function — one call evaluates all of the
    /// phase's drivers, reading the live state store directly (no
    /// gather, no per-driver indirect calls, no per-eval checks) — then
    /// commit each result through the same per-driver commit body the
    /// interpreter uses (`commit_driver_bits`).  Returns false when the
    /// phase has no compiled function (the caller falls through to the
    /// unchanged interpreter replay).
    ///
    /// Sound: the phase is terminal (validated by the recorder), so its
    /// input bits are frozen for the whole settlement and computing all
    /// drivers before any commit is exactly what the interleaved
    /// interpreter computes.  Takes the engine's fields as disjoint
    /// borrows (the caller holds the schedule's driver list live
    /// through the phase check) and stays out of line, out of
    /// `cycle_settlement`'s machine code, so the feature-on build's
    /// inactive footprint matches the non-jit build's.
    #[cfg(feature = "jit")]
    #[inline(never)]
    fn jit_phase_replay(
        circuit: &Circuit,
        program: &Program,
        state: &mut State,
        schedule: &mut CycleSchedule,
        jit: &mut super::PhaseJit,
        pos: usize,
    ) -> bool {
        if !jit.is_compiled(pos) {
            return false;
        }
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_jit_phase_call();
        let drivers = schedule.phase_drivers(pos);
        let results = jit.run(pos);
        for (k, &d) in drivers.iter().enumerate() {
            let comp = SignalId::from_index(d as usize);
            let (typ, width) = program.driver_out(comp);
            Self::commit_driver_bits(circuit, state, comp, typ, width, results[k]);
        }
        state.clear_dirty();
        schedule.advance();
        true
    }
    /// The bits-only commit body for one driver's computed raw bits,
    /// shared by the interpreter path and the JIT's batch path: a
    /// register commits its pending next-value (`State::set_reg_bits`,
    /// compared raw at transfer time); a combinational driver takes the
    /// bits-only commit path (`transfer_clock_bits_split`: edge
    /// detection from the pre-write live value, then a raw-integer
    /// compare in `State::set_val_bits`).  Extracted verbatim from
    /// `eval_and_commit_split`'s `RunOut::Bits` arm so the JIT's batch
    /// results commit through the exact protocol the interpreter's
    /// would.
    #[inline(always)]
    fn commit_driver_bits(
        circuit: &Circuit,
        state: &mut State,
        comp: SignalId,
        typ: TypeId,
        width: Width,
        bits: u64,
    ) -> bool {
        if circuit.registers[comp.index()] {
            state.set_reg_bits(comp, typ, width, bits);
            false
        } else {
            Self::transfer_clock_bits_split(circuit, state, comp, typ, width, bits)
        }
    }
    /// `eval_and_commit`'s commit body over pre-split field borrows
    /// (see `eval_driver_out_split`); no epoch machinery.
    #[inline]
    fn eval_and_commit_split(
        circuit: &Circuit,
        program: &Program,
        vm: &mut Vm,
        state: &mut State,
        comp: SignalId,
    ) -> bool {
        if let Some(peer_id) = circuit.aliases[comp.index()] {
            let new_value = state.get(peer_id);
            if circuit.registers[comp.index()] {
                // A register's pending next-value only commits at a
                // clock edge: copying the peer writes no live value and
                // starts no propagation.
                state.set_reg(comp, new_value);
                false
            } else {
                let old = state.get(comp);
                Self::transfer_clock_split(circuit, state, comp, &old, new_value)
            }
        } else {
            match Self::eval_driver_out_split(program, vm, state, comp) {
                RunOut::Bits { typ, width, bits } => {
                    // The bits-only commit path: identical for the
                    // interpreter's and the JIT's raw bits (see
                    // `commit_driver_bits`).
                    Self::commit_driver_bits(circuit, state, comp, typ, width, bits)
                }
                RunOut::Tagged(new_value) => {
                    if circuit.registers[comp.index()] {
                        state.set_reg(comp, new_value);
                        false
                    } else {
                        // The tagged commit path must not call `set`
                        // (the orchestrator: flow + edge transfers +
                        // watchers), which would recurse re-entrantly
                        // into the loop we're in.  Edge detection reads
                        // the live value *before* the write, so `old`
                        // is captured at this call site.
                        let old = state.get(comp);
                        Self::transfer_clock_split(circuit, state, comp, &old, new_value)
                    }
                }
            }
        }
    }

    /// Commit a rising edge of `clock`'s clock domain, in two phases
    /// so that no member of the domain ever sees another member's
    /// post-edge value — the nonblocking semantics of the emitted
    /// Verilog (the VM twin of `LegacySim::commit_domain`).
    ///
    /// Phase 1 evaluates every memory's write site — (enable,
    /// address, data) — against pre-edge values and stages the
    /// resulting writes (registers need no staging: their pendings
    /// were already sampled pre-edge).  Phase 2 then transfers the
    /// registers and applies the staged memory writes together.
    /// Committing member by member instead would let a memory
    /// declared after a register sample that register's post-edge
    /// value, making a write site's sampling depend on declaration
    /// order.
    ///
    /// Each member commits at most once per settlement (the
    /// `mark_committed` guard), so a clock alias rising again later
    /// in the same flow re-runs both phases as a no-op.  Returns the
    /// domain's member count.  Split-field form (circuit and state
    /// instead of `&mut self`), matching the `transfer_clock*` call
    /// convention.
    #[inline]
    fn commit_domain_split(circuit: &Circuit, state: &mut State, clock: SignalId) -> usize {
        let m = circuit.clock_sensitivity_count(clock);
        // Phase 1: stage memory write sites against pre-edge values.
        // Skipped outright for memory-free designs (the common case,
        // and the per-cycle hot path).
        let mut any_memory = false;
        for i in 0..m {
            let id = circuit.clock_sensitivity_at(clock, i);
            if circuit.is_memory(id) {
                any_memory = true;
                Self::stage_memory_write_split(circuit, state, id);
            }
        }
        // Phase 2: transfer the registers, then apply the staged
        // writes.  Neither reads live values, so the two loops cannot
        // interact.
        for i in 0..m {
            let id = circuit.clock_sensitivity_at(clock, i);
            if !any_memory || !circuit.is_memory(id) {
                state.transfer(id);
            }
        }
        if any_memory {
            for (mem_id, index, value) in state.drain_pending_memory_writes() {
                state.set_memory_word(mem_id, index, value);
            }
        }
        m
    }

    /// Stage a memory's write site for the current clock-edge commit:
    /// sample the (enable, address, data) expressions against pre-edge
    /// values and queue the write (`State::stage_memory_write`)
    /// without touching the words (read-during-write sees the old
    /// value because the words are only updated by phase 2 of
    /// `commit_domain_split`; at most one write fires — overlapping
    /// guards are rejected by analysis).  The write-site
    /// expressions run through the shared tree-walking evaluator —
    /// three small evaluations once per cycle per memory, not per
    /// driver — against either engine's state via `ComponentValues`.
    fn stage_memory_write_split(circuit: &Circuit, state: &mut State, mem_id: SignalId) {
        // At most one commit per settlement, like a register: a clock
        // alias rising again later in the same flow must not re-write
        // with post-edge values.
        if !state.mark_committed(mem_id) {
            return;
        }
        let (enable, address, data) = {
            let writes = &circuit.memory(mem_id).writes;
            (
                writes.enable.clone(),
                writes.address.clone(),
                writes.data.clone(),
            )
        };
        let context =
            crate::sim::legacy::eval::context_for_values(circuit, state, mem_id);
        if !enable.eval(&context).get_bit(0) {
            return;
        }
        let index = address.eval(&context).to_u64() as usize;
        let value = data.eval(&context);
        state.stage_memory_write(mem_id, index, value);
    }

    /// The live words of memory `signal_id`, or `None` for any other
    /// component (surfaces memory words to `vir poke`).
    pub(crate) fn memory_words(&self, signal_id: SignalId) -> Option<std::sync::Arc<Vec<Value>>> {
        if self.circuit.is_memory(signal_id) {
            Some(self.state.memory_words(signal_id))
        } else {
            None
        }
    }

    /// Clock-transfer commit primitive, shared by `flow`'s
    /// mid-propagation path and `set_without_flow`: detect a rising
    /// edge (`old` vs `new`), write `new` to `signal_id`'s val node,
    /// and — if the write is a rising edge on a clock wire (e.g. a
    /// submodule port driven by `clk`) — transfer the registers it
    /// clocks, the same thing `set()` does for the top-level clock.
    ///
    /// Edge detection bases stay at the call sites: each reads the live
    /// value *before* the write and hands it in as `old`.
    ///
    /// Split-field form (circuit and state instead of `&mut self`): the
    /// flow loop calls this while the circuit is borrowed immutably.
    #[inline]
    fn transfer_clock_split(
        circuit: &Circuit,
        state: &mut State,
        signal_id: SignalId,
        old: &Value,
        new: Value,
    ) -> bool {
        let rising = is_rising(old, &new);
        // Rising edges on clock wires are the per-cycle heartbeat:
        // one per cycle for a single-clock design.  The sampler tick
        // doubles as the rising-edge count, and every 8th edge's
        // transfer loop (set_val + the register transfers) is timed.
        #[cfg(feature = "sim-profile")]
        let _profile_sample = if rising {
            crate::sim::profile::sample_xfer_loop()
        } else {
            None
        };
        let wrote = state.set_val(signal_id, new);
        if rising {
            let n = Self::commit_domain_split(circuit, state, signal_id);
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_transfers(n as u64);
            #[cfg(not(feature = "sim-profile"))]
            let _ = n;
        }
        wrote
    }
    /// The narrow bits-only commit path (PERF-IDEAS item 10):
    /// `transfer_clock`'s raw-integer twin for narrow drivers.  The
    /// same sequence, no `Value`s until a write: edge detection from
    /// the pre-write live value (the same predicate `is_rising`
    /// applies, over pre-extracted parts), the write through
    /// `State::set_val_bits` (a raw compare; the tagged `Value` is
    /// built only on an actual write), and the rising-edge
    /// register-transfer loop.  Narrow registers' transfers themselves
    /// compare raw bits (`State::transfer`'s bits path).
    ///
    /// Split-field form (circuit and state instead of `&mut self`): the
    /// flow loop calls this while the circuit is borrowed immutably.
    #[inline]
    fn transfer_clock_bits_split(
        circuit: &Circuit,
        state: &mut State,
        signal_id: SignalId,
        typ: TypeId,
        width: Width,
        bits: u64,
    ) -> bool {
        // Edge detection reads the live value *before* the write, the
        // same bases `transfer_clock` uses.
        let rising = {
            let old = state.peek(signal_id);
            is_rising_bits(old.width(), old.get_bit(0), bits & 1 == 1)
        };
        // Rising edges on clock wires are the per-cycle heartbeat:
        // one per cycle for a single-clock design.  The sampler tick
        // doubles as the rising-edge count, and every 8th edge's
        // transfer loop (set_val + the register transfers) is timed.
        #[cfg(feature = "sim-profile")]
        let _profile_sample = if rising {
            crate::sim::profile::sample_xfer_loop()
        } else {
            None
        };
        let wrote = state.set_val_bits(signal_id, typ, width, bits);
        if rising {
            let n = Self::commit_domain_split(circuit, state, signal_id);
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_transfers(n as u64);
            #[cfg(not(feature = "sim-profile"))]
            let _ = n;
        }
        wrote
    }

    pub(crate) fn set_without_flow(&mut self, signal_id: SignalId, value: Value) {
        let old = self.state.get(signal_id);
        Self::transfer_clock_split(&self.circuit, &mut self.state, signal_id, &old, value);
    }

    pub(crate) fn get(&self, signal_id: SignalId) -> Value {
        self.state.get(signal_id)
    }
    /// The whole-design cycle plan verdict, once the primary clock has
    /// been attached: the static schedule data (eval regions, the
    /// transfer point, the combinational tail) for a qualified design,
    /// or the precise disqualification reason.  `None` before the
    /// first `attach_clock`.  Internal: the engine does not act on the
    /// plan in this phase (tests inspect it; a later phase consumes it).
    #[cfg_attr(not(test), allow(dead_code))]
    pub(in crate::sim) fn cycle_plan(&self) -> Option<&CyclePlanResult> {
        self.cycle_plan.as_ref()
    }

    /// The cycle plan verdict as a machine-readable string:
    /// `"qualified"` or `"disqualified(reason)"`, or `"pending"` when
    /// no clock has been attached yet.  For tests and `simprof`.
    pub(crate) fn cycle_plan_verdict(&self) -> String {
        match self.cycle_plan.as_ref() {
            Some(plan) => plan.verdict(),
            None => "pending".to_string(),
        }
    }

    /// The cycle plan verdict with any disqualification decoded to
    /// full signal names (the offending component's elaborated path),
    /// for `simprof` and load-time diagnostics.  `"pending"` before
    /// the first `attach_clock`.
    pub(crate) fn cycle_plan_diagnostic(&self) -> String {
        match self.cycle_plan.as_ref() {
            None => "pending".to_string(),
            Some(CyclePlanResult::Qualified(_)) => "qualified".to_string(),
            Some(CyclePlanResult::Disqualified(d)) => match d {
                Disqualification::RegisterClockedByNonPrimary { register, clock, primary } => {
                    format!(
                        "disqualified({}): register {} clocked by {} (primary clock {})",
                        d.reason(),
                        self.full_name(*register),
                        self.full_name(*clock),
                        self.full_name(*primary),
                    )
                }
                Disqualification::CyclicCombinationalGraph => format!(
                    "disqualified({})",
                    d.reason(),
                ),
                Disqualification::UntranslatableDriver { driver, instruction } => {
                    format!(
                        "disqualified({}): driver {} uses instruction {instruction}",
                        d.reason(),
                        self.full_name(*driver),
                    )
                }
            },
        }
    }

    /// Disassemble every compiled driver program in human-readable
    /// form: one section per driven component, branch targets as
    /// labels, signal references by path. The format `vir run
    /// --bytecode` prints and `vm/INSTRUCTIONS.md` documents.
    pub(crate) fn dump_bytecode(&self) -> String {
        let mut names = vec![bstr::BString::default();
            self.elaboration().components().len()];
        for comp in self.elaboration().components() {
            names[comp.id().index()] = comp.path().clone();
        }
        super::disassemble(&self.program, &names)
    }

    pub(crate) fn changed_since_dispatch(&self) -> bool {
        self.state.changed_since_dispatch()
    }

    /// Whether the program carries a static topological schedule (the
    /// stage-A schedule; a cyclic driver graph leaves it off).
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn has_topo_schedule(&self) -> bool {
        self.program.topo_schedule().is_some()
    }

    pub(crate) fn changed_signal_count(&self) -> usize {
        self.state.changed_signal_count()
    }

    pub(crate) fn changed_signal_ids(&self) -> Box<dyn Iterator<Item = SignalId> + '_> {
        self.state.changed_signal_ids()
    }

    pub(crate) fn clear_changed_since_dispatch(&mut self) {
        self.state.clear_changed_since_dispatch();
    }

    pub(crate) fn set_watched(&mut self, signal_id: SignalId, watched: bool) {
        self.state.set_watched(signal_id, watched);
    }

    pub(crate) fn clear_watched(&mut self) {
        self.state.clear_watched();
    }

    /// Hook for structural changes (watcher registration/deregistration,
    /// `finish()` pruning): invalidate the settlement schedule caches —
    /// both stages are keyed on the circuit's sensitivity structure,
    /// which a structural change invalidates.
    pub(crate) fn on_structural_change(&mut self) {
        // Stage B: the cache is keyed on the circuit's sensitivity
        // structure, which watcher registration/deregistration changes
        // (the visited-driver set for a given worklist is a function of
        // the structural sensitivity, not just the dirty set).
        self.schedule_cache.clear();
        // Stage C: the deep-cycle phases are functions of the same
        // structural invariants.
        self.cycle_schedule.reset();
    }

    /// Hook for the first clock attachment: compute the whole-design
    /// cycle plan (the qualification checker's clock clause compares
    /// every register's clock sensitivity against the primary
    /// free-running clock, which is only known at the first
    /// `attach_clock`).  The verdict is recorded, unused; the engine
    /// continues exactly as today.
    pub(crate) fn on_clock_attached(&mut self, signal: SignalId) {
        if self.cycle_plan.is_none() {
            let plan = super::cycle::plan(&self.circuit, &self.program, signal);
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::set_cycle_plan_status(plan.status_code());
            self.cycle_plan = Some(plan);
        }
    }

    pub(crate) fn full_name(&self, signal_id: SignalId) -> bstr::BString {
        self.elaboration().component(signal_id).path().clone()
    }

    /// The interned [`TypeId`] of `signal_id`'s type — used by the
    /// JIT's activation-time `Value::jit_layout` probe.
    #[cfg(feature = "jit")]
    fn component_type_id(&self, signal_id: SignalId) -> crate::sim::layout::TypeId {
        self.circuit
            .type_layout_index
            .type_id(&self.elaboration().component(signal_id).typ())
    }
}


const EPOCH_CHECK_MIN_DIRTY: usize = 32;

/// 0→1 transition on a 1-bit signal, over pre-extracted parts: the
/// shared predicate behind `is_rising` (the `Value` commit path) and
/// `transfer_clock_bits`'s raw-bits path (the new side there is the
/// raw result bits, whose bit 0 is what [`Value::get_bit`] would read
/// on the masked narrow value).
fn is_rising_bits(old_width: Width, old_bit0: bool, new_bit0: bool) -> bool {
    old_width == 1 && !old_bit0 && new_bit0
}

/// 0→1 transition on a 1-bit signal.  Keeps `set()`'s edge detection
/// safe on non-clock (and multi-bit) signals.
fn is_rising(old: &Value, new: &Value) -> bool {
    is_rising_bits(old.width(), old.get_bit(0), new.get_bit(0))
}