//! The `Sim` runtime: combinational propagation (`flow`), register
//! transfer on clock edges, a `SimLock` for batching writes, clock
//! attachment, and the `run` loop draining the scheduler and stepping
//! free-running clock edges, plus elaboration lookup and callback
//! registration helpers (`at`, `after`, `on_change`, `on_clock`).

use crate::analysis::symbols::SymbolId;
use crate::analysis::elaboration::{SignalId, Elaboration};
use crate::common::{ComponentKind, Width};
use crate::db::Db;
use crate::sim::bits::BitPat;
use crate::sim::circuit::Circuit;
use crate::sim::layout::{TypeId, TypeLayoutIndex};
use crate::sim::scheduler::{Callback, Event, EventCallback, Scheduler};
use crate::sim::state::State;
use crate::sim::value::Value;
use crate::sim::vm;
use crate::types::Type;
use std::cell::RefCell;
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use bstr::{BStr, BString};

/// Strategy for choosing the value of uninitialized registers and
/// top-level incoming ports.  There are no X or Z values: every cell
/// always carries a fully-defined bit pattern.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum InitStrategy {
    /// Pseudorandom bit patterns (seeded from the wall clock).
    #[default]
    Random,
    /// Pseudorandom bit patterns, seeded from an explicit value: two
    /// sims built with the same seed over the same design carry
    /// bit-identical uninitialized values, so their traces are
    /// directly comparable (differential testing).
    Seeded(u64),
}

impl InitStrategy {
    /// The seed for the uninitialized-value RNG: a fresh wall-clock
    /// value for `Random`, the given value for `Seeded`.
    pub(super) fn rng_seed(&self) -> u64 {
        match self {
            InitStrategy::Random => initial_seed(),
            InitStrategy::Seeded(seed) => *seed,
        }
    }

    pub(super) fn value(&self, typ_id: TypeId, width: Width, rng: &mut u64) -> Value {
        match self {
            InitStrategy::Random | InitStrategy::Seeded(_) =>
                Value::new(typ_id, BitPat::random(width, rng)),
        }
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


/// A lock that defers circuit settlement until dropped.
/// While held, call `.set()` to accumulate signal changes without flowing.
pub struct SimLock<'a> {
    sim: &'a mut Sim,
}

impl<'a> Deref for SimLock<'a> {
    type Target = Sim;

    fn deref(&self) -> &Self::Target {
        self.sim
    }
}

impl<'a> SimLock<'a> {
    /// Write `value` to `signal_id`. Settlement is deferred until this lock is dropped.
    pub fn set(&mut self, signal_id: SignalId, value: Value) {
        self.sim.set_without_flow(signal_id, value);
    }
}

impl<'a> Drop for SimLock<'a> {
    fn drop(&mut self) {
        self.sim.unlock();
    }
}

pub struct Sim {
    circuit: Circuit,
    /// Compiled drivers: one code range per driven component.  Every
    /// driver lowers to bytecode; there is no interpreter fallback.
    program: vm::Program,
    /// The frame pool backing `program`'s drivers.
    vm: vm::Vm,
    state: State,
    sched: Scheduler,
    /// Free-running clocks registered by `attach_clock`.  Each edge is
    /// stepped directly by the run loop when it beats every queued
    /// user event; see `ClockHandle` and `Sim::run`.
    clocks: Vec<ClockHandle>,
    /// Persistent watcher table (`on_change`/`on_clock`): registered
    /// once, swept in place at each settlement.  No per-fire
    /// re-registration, no queue rebuild.
    watchers: Vec<Watcher>,
    /// Watchers bucketed by the signal they observe:
    /// Watcher buckets, densely indexed by the observed signal's
    /// index: `watcher_buckets[signal_index]` lists the registration
    /// indices (into `watchers`) of every watcher observing that
    /// signal, in registration order.  Built by `on_change`/`on_clock`
    /// (registration appends to the table and to the signal's bucket)
    /// and rebuilt after the shutdown retain prunes dead entries, so
    /// the buckets always cover exactly the live table.  A dispatch
    /// pass visits only the buckets of signals that changed since the
    /// last sweep (or the table itself, when the changed set
    /// outnumbers the table), so the sweep cost follows what could
    /// actually fire, not the table size.
    watcher_buckets: Vec<Vec<usize>>,
    /// Reusable queue of registration indices for one bucket-walk
    /// dispatch pass: the union of the changed signals' buckets,
    /// sorted ascending.  Cleared and refilled each pass, never
    /// reallocated.
    dispatch_queue: Vec<usize>,
    /// True while the watcher dispatch loop is running.  A nested
    /// `unlock()` (a callback's own `lock()`/`set()`/`unlock()`) must
    /// only settle the circuit (`flow`), never re-sweep the table —
    /// dispatch happens exactly once per settlement, in the outermost
    /// unlock.
    dispatching: bool,
    /// Per-driver last-eval epoch (see `vm::schedule` and
    /// `State::epoch`): `last_run[component]` is the write-counter
    /// snapshot a driver's last evaluation was taken against.  A repeat
    /// visit whose trigger epoch predates it is skipped: the driver
    /// already observed that write, so its committed output is the
    /// value it would recompute.  Zero-initialized, then primed to the
    /// construction-time write count after `flow_all`.
    last_run: Vec<u64>,
}

/// One free-running clock registered by `Sim::attach_clock`.
///
/// Replaces the self-re-arming `AfterDelay` toggle event the engine
/// used to schedule per half period: the run loop steps an edge
/// directly whenever it beats every queued user event, so a clock
/// cycle costs no heap push (with its boxed-callback allocation), no
/// heap pop, and no scheduler dispatch.
///
/// `next` is the pending edge's `(time_ps, seq)` pair, compared in the
/// scheduler heap's own order — earliest time, then FIFO sequence —
/// so a tie against a user event fires in exactly the order the edge
/// would have had as a heap entry.  It is `None` only while the edge
/// is being processed, mirroring the old heap state during the toggle
/// callback: the popped entry was gone and the next one was registered
/// only after the settlement, so a re-entrant `run()` from a
/// settlement callback saw no pending edge for this clock.
struct ClockHandle {
    signal: SignalId,
    half_period_ps: u64,
    next: Option<(u64, u64)>,
}

/// How a watcher decides to fire.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WatchKind {
    /// Fire when the observed value differs from the last one this
    /// watcher saw (`on_change`).
    Change,
    /// Fire on a 0→1 transition of bit 0 (`on_clock`).
    ClockRising,
}

/// One persistent entry in `Sim`'s watcher table.
struct Watcher {
    signal: SignalId,
    kind: WatchKind,
    /// The last value *this watcher* observed.  Per-watcher (not shared
    /// circuit state), so each watcher observes each value change
    /// exactly once, regardless of settlement/commit ordering.
    last: Value,
    /// False once the callback ran after `finish()` was requested: the
    /// watcher must not survive into a subsequent `run()`.
    alive: bool,
    /// `Rc<RefCell<_>>` so the callback can be invoked with `&mut Sim`
    /// while the table itself is only borrowed (the engine never
    /// re-registers a watcher; the shared cell exists purely for the
    /// borrow checker).
    callback: Rc<RefCell<Callback>>,
}

/// A clock timing specification.  Holds only the period; the signal to
/// drive is supplied separately when calling [`Sim::attach_clock`].
/// Construct with [`Clock::with_period_ps`] or [`Clock::with_freq_hz`].
#[derive(Debug, Clone, Copy)]
pub struct Clock {
    pub period_ps: u64,
}

impl Clock {
    /// Specify the clock by its full period in picoseconds.
    pub fn with_period_ps(period_ps: u64) -> Clock {
        Clock { period_ps }
    }

    /// Specify the clock by its frequency in Hz.  Converts to picoseconds
    /// via `1_000_000_000_000 / freq_hz`; the period is truncated to the
    /// nearest picosecond.
    pub fn with_freq_hz(freq_hz: u64) -> Clock {
        let period_ps = 1_000_000_000_000u64 / freq_hz;
        Clock { period_ps }
    }
}

/// The decoded constructor of a union/enum [`Value`]: which
/// constructor was selected, and the typed payloads it carries.
///
/// Returned by [`Sim::decode_ctor`].  This is the public face of the
/// type-layout machinery: callers never see `TypeLayoutIndex`,
/// `TypeId`, or raw bit patterns.
#[derive(Clone, Debug)]
pub struct Ctor {
    /// The selected constructor's name, e.g. `"Valid"`.
    pub name: String,
    /// The constructor's data, one [`Value`] per type parameter, in
    /// declaration order.  Each payload carries its parameter's type,
    /// so it can be compared with `==` or, if it is itself a
    /// union/enum, decoded further with [`Sim::decode_ctor`].  Empty
    /// for constructors with no data (e.g. `Invalid`).
    pub payloads: Vec<Value>,
}

#[derive(Debug)]
pub enum SimError {}

impl std::fmt::Debug for Sim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Sim")
            .field("scheduler", &self.sched)
            .finish_non_exhaustive()
    }
}

impl Sim {
    pub fn new(db: &Db, top: SymbolId, strategy: InitStrategy) -> Sim {
        let circuit = Circuit::new(db, top);
        let program = vm::compile_program(&circuit);
        let vm_pool = vm::Vm::new(&program);

        let n_components = circuit.elaboration.components().len();
        let mut sim = Sim {
            state: State::new(circuit.elaboration.as_ref(), &circuit.type_layout_index, &strategy),
            circuit,
            program,
            vm: vm_pool,
            sched: Scheduler::new(),
            clocks: Vec::new(),
            watchers: Vec::new(),
            watcher_buckets: Vec::new(),
            dispatch_queue: Vec::new(),
            dispatching: false,
            last_run: vec![0; n_components],
        };

        sim.apply_initials();
        sim.flow_all();
        // Prime the last-eval epochs: `flow_all` ran every
        // driver to a fixpoint over the settled values, so each driver's
        // committed output already reflects every write up to now; any
        // later write carries a larger epoch and forces re-evaluation.
        let priming_epoch = sim.state.epoch_clock();
        if let Some(schedule) = sim.program.topo_schedule() {
            for &driver in schedule.order() {
                sim.last_run[driver as usize] = priming_epoch;
            }
        }
        sim
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
            let value = vm::compile_initial_value(&self.circuit, &expr);
            self.state.set_val(signal_id, value);
        }
        self.flow();
    }

    pub(super) fn type_layout_index(&self) -> &TypeLayoutIndex {
        &self.circuit.type_layout_index
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
    fn eval_driver_out(&mut self, comp: SignalId) -> vm::RunOut {
        Self::eval_driver_out_split(&self.program, &mut self.vm, &self.state, comp)
    }

    /// `eval_driver_out` over pre-split field borrows: `flow` holds
    /// the circuit as an immutable borrow while committing per driver,
    /// so the per-driver seam must work through disjoint
    /// `program`/`vm`/`state` borrows instead of `&mut self` methods.
    #[inline]
    fn eval_driver_out_split(
        program: &vm::Program,
        vm: &mut vm::Vm,
        state: &State,
        comp: SignalId,
    ) -> vm::RunOut {
        // Constant-unit skip (synthesis graft 2): a driver the
        // optimizer folded entirely to a constant commits that value
        // directly, `flow`/`flow_all` skip the run.  Constants stay on
        // the `Value` path: the folded value is already built.
        if let Some(value) = program.const_value(comp).cloned() {
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_const_skip();
            return vm::RunOut::Tagged(value);
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

    /// Decode a value whose type is a union or enum into the
    /// constructor it currently is.  Returns `None` when the value's
    /// type is not a union/enum (e.g. a plain `Word` or `Bit` — there
    /// is no "variant" to decode), or the bit pattern is not a valid
    /// inhabitant of the type (unknown tag, non-zero padding above the
    /// payload; mid-simulation this can happen transiently, before the
    /// logic has settled).
    pub fn decode_ctor(&self, value: &Value) -> Option<Ctor> {
        let (slot, payload_bits) = self
            .circuit
            .type_layout_index
            .decode_ctor(value.typ_id(), &value.bitpat())?;
        let payloads = slot
            .param_types
            .iter()
            .zip(payload_bits)
            .map(|(typ, bits)| Value::new(self.circuit.type_layout_index.type_id(typ), bits))
            .collect();
        let name = String::from_utf8(slot.name.to_vec()).unwrap_or_else(|_| {
            panic!("constructor name was not valid UTF-8: {:?}", slot.name)
        });
        Some(Ctor { name, payloads })
    }

    /// Stream a VCD trace of every dumpable signal to `writer`.  The header
    /// and an initial `$dumpvars` snapshot are written synchronously; per-
    /// signal `on_change` watchers are registered to write subsequent value
    /// changes.  A final `flush()` is registered to fire from `Sim::run`'s
    /// end-of-simulation hook.  See `vcd.rs` for the bit-layout seam.
    pub fn attach_vcd<W: Write + 'static>(&mut self, writer: W) {
        crate::sim::vcd::attach(self, writer);
    }

    /// Evaluate every driven component, repeating until the
    /// combinational network reaches a fixpoint.
    ///
    /// Used at construction: there are no X values, so a component whose
    /// random-initialized bits happen to equal its computed value would
    /// never look "changed" and its dependents would keep stale random
    /// values.  A full-network pass makes every value consistent with
    /// the (initial) register values before the run starts.
    fn flow_all(&mut self) {
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
    }

    pub fn tick(&mut self, signal_id: SignalId) {
        self.tick_prop(signal_id);
        self.flow();
    }

    fn tick_prop(&mut self, signal_id: SignalId) {
        // Iterate by index against the immutable Circuit so we can recurse
        // mutably into `self.tick` / `self.transfer` without cloning the
        // sensitivity IndexSet on every step.
        let n = self.circuit.sensitivity_count(signal_id);
        for i in 0..n {
            let inner = self.circuit.sensitivity_at(signal_id, i);
            self.tick(inner);
        }

        let m = self.circuit.clock_sensitivity_count(signal_id);
        for i in 0..m {
            let reg_signal_id = self.circuit.clock_sensitivity_at(signal_id, i);
            self.state.transfer(reg_signal_id);
        }
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_transfers(m as u64);
    }

    fn flow(&mut self) {
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
        while let Some(signal_id) = self.state.pop_dirty() {
            let n = self.circuit.sensitivity_count(signal_id);
            for i in 0..n {
                let update_signal_id = self.circuit.sensitivity_at(signal_id, i);
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
        program: &vm::Program,
        vm: &mut vm::Vm,
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

    /// `eval_and_commit`'s commit body over pre-split field borrows
    /// (see `eval_driver_out_split`); no epoch machinery.
    #[inline]
    fn eval_and_commit_split(
        circuit: &Circuit,
        program: &vm::Program,
        vm: &mut vm::Vm,
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
                vm::RunOut::Bits { typ, width, bits } => {
                    if circuit.registers[comp.index()] {
                        // A register's driver computes its
                        // *next* value; the current value only
                        // changes on a clock edge.  Narrow
                        // registers store the pending next-value
                        // as raw bits (`State::set_reg_bits`),
                        // compared raw at transfer time.
                        state.set_reg_bits(comp, typ, width, bits);
                        false
                    } else {
                        // The bits-only commit path for narrow
                        // drivers: edge detection from the
                        // pre-write live value, then a
                        // raw-integer compare in
                        // `State::set_val_bits`.
                        Self::transfer_clock_bits_split(circuit, state, comp, typ, width, bits)
                    }
                }
                vm::RunOut::Tagged(new_value) => {
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

    /// Settle the circuit after a batch of writes: propagate (`flow`),
    /// then dispatch value-change watchers.
    ///
    /// The sweep is skipped entirely when no watched signal's stored
    /// value has changed since the last sweep that observed everything
    /// (see `State::changed_since_dispatch`): no watcher can fire over
    /// values nobody wrote to.
    ///
    /// Dispatch is single-phase and non-reentrant: a nested `unlock()`
    /// (from a watcher callback's own `lock()`/`set()`/`unlock()`) only
    /// runs `flow()`; the sweep happens exactly once, in the outermost
    /// unlock.  This makes double-firing structurally impossible — the
    /// old implementation re-ran the watcher scan inside nested unlocks
    /// while transitions were still uncommitted, re-draining freshly
    /// re-registered watchers (one extra callback fire per event).
    fn unlock(&mut self) {
        self.flow();

        if self.dispatching {
            return;
        }

        // Skip the sweep entirely when no watched signal's stored value
        // has changed since the last sweep pass that observed
        // everything: a watcher fires only on a value change (`Change`)
        // or a 0→1 transition of bit 0 (`ClockRising`), and both
        // require the observed signal's stored value to have changed,
        // so a settlement with no write to a watched signal cannot fire
        // anything.  The flag is raised by `State`'s two write
        // primitives (`set_val`, `transfer`) on every write to a
        // watched signal (watched-only marking: unwatched signals have
        // no watcher to observe them, so their writes skip the
        // changed-set bookkeeping entirely), including
        // `set_without_flow` (via `transfer_clock`) and the register
        // transfers inside this sweep's own callbacks; each sweep pass
        // consumes it together with the per-signal changed set.
        if self.state.changed_since_dispatch() {
            self.dispatching = true;
            // Sweep-and-fire loop: each pass fires, in registration
            // order, every watcher whose observed value differs from the
            // last value it saw — visiting only the watchers whose
            // signals were actually written since the previous pass
            // started.  Callbacks may drive signals (settled by their
            // nested flow-only unlocks) or register more watchers; the
            // loop ends when the changed set is empty, i.e. when a pass
            // has observed everything with no fresh writes — the same
            // quiescence the old fired-until-nothing loop reached,
            // without re-walking watchers over changes they already
            // observed.  The loop runs to quiescence even past
            // `finish()`: watchers firing after the shutdown request die
            // instead of persisting (as the old re-arm check did), but
            // their final cascades still fire.
            while self.state.changed_signal_count() != 0 {
                self.dispatch_pass();
            }
            self.dispatching = false;
        }

        // Watchers whose callback ran after `finish()` was requested do
        // not survive into a subsequent `run()` (the old one-shot queue
        // achieved this by skipping the re-register step).  Skipped
        // sweeps cannot affect it: a watcher only becomes dead inside
        // `dispatch_pass`, and a skipped sweep runs no callback, so the
        // table is unchanged either way.
        if self.sched.is_shutdown_requested() {
            self.watchers.retain(|w| w.alive);
            self.rebuild_buckets();
        }
    }

    /// One sweep-and-fire pass over the watcher table.  Only the
    /// watchers whose observed signal has an actual write pending
    /// since the previous pass started are visited, in registration
    /// order.  Each visited watcher compares the live value against
    /// its own last-seen value; watchers that observe a change fire
    /// (for `ClockRising`, only on a 0→1 transition of bit 0), each
    /// updating its own last-seen value as it fires.
    ///
    /// Order-equivalence with the old whole-table sweep: a watcher can
    /// only fire (or need its `last` refreshed) over a signal someone
    /// actually wrote since the watcher's last observation, and every
    /// write to a watched signal marks its signal in the changed set
    /// (watched-only marking: unwatched signals have no watcher, so
    /// they never need an entry).  So the fired
    /// watchers are exactly the whole-table sweep's, and both walks go
    /// in ascending registration index: the bucket walk sorts the
    /// union of the changed signals' buckets; the table walk goes in
    /// table order.  Firing order among simultaneously-changed
    /// watchers is therefore identical to the unbucketed sweep,
    /// including when watchers on different signals change in the same
    /// settlement.
    ///
    /// The pass consumes the changed set at its start: it observes the
    /// watchers whose signals somebody wrote since the previous pass
    /// started, and any write landing while it runs (callbacks and
    /// their nested settlements) re-marks the set for the next pass.
    /// Two strategies, chosen per pass by whichever set is smaller:
    /// the bucket walk visits the changed signals' buckets (cost
    /// proportional to the watched signals that changed — the set
    /// carries watched signals only), collapsing
    /// the sweep toward watcher-fire cost on designs where few signals
    /// change per settlement; the table walk visits every watcher and
    /// pays the value compare (cost proportional to the table), which
    /// is the cheaper side on chain designs where most signals change
    /// every cycle but few have watchers.  Never worse than the old
    /// whole-table sweep, which paid a full value compare per table
    /// entry per pass.
    fn dispatch_pass(&mut self) {
        // One `Instant` pair per sweep pass; the pass covers only the
        // changed signals' buckets or the table, so the read cost stays
        // proportionate to what could actually fire.
        #[cfg(feature = "sim-profile")]
        let _profile_phase = crate::sim::profile::time_dispatch();
        if self.state.changed_signal_count() < self.watchers.len() {
            // Bucket walk: visit only the changed signals' buckets, in
            // registration order (ascending index across buckets and
            // within each).
            self.dispatch_queue.clear();
            for signal_id in self.state.changed_signal_ids() {
                let index = signal_id.index();
                if let Some(bucket) = self.watcher_buckets.get(index) {
                    self.dispatch_queue.extend_from_slice(bucket);
                }
            }
            self.dispatch_queue.sort_unstable();
            // The queue now covers every watcher whose signal was
            // written since the previous pass started.  Consuming the
            // set here, before any callback runs, lets the pass's own
            // writes re-mark it for the next pass; the dispatch loop
            // ends when the set is empty, since a pass over an empty
            // set would observe nothing.
            self.state.clear_changed_since_dispatch();
            let mut table_len = self.watchers.len();
            let mut qi = 0;
            while qi < self.dispatch_queue.len() {
                let i = self.dispatch_queue[qi];
                qi += 1;
                self.dispatch_visit(i);
                // Watchers registered by callbacks during this pass are
                // visited at the end of this same pass: their indices
                // are larger than every queued one, so the queue stays
                // ascending and registration order is preserved —
                // exactly as the whole-table sweep did.
                while table_len < self.watchers.len() {
                    self.dispatch_queue.push(table_len);
                    table_len += 1;
                }
            }
        } else {
            // Table walk: every watcher, in table (registration) order
            // — the old whole-table sweep, which is cheaper than
            // queueing a changed set no smaller than the table.
            self.state.clear_changed_since_dispatch();
            let mut i = 0;
            while i < self.watchers.len() {
                self.dispatch_visit(i);
                i += 1;
            }
        }
    }

    /// Visit one watcher: compare the live value against its own
    /// last-seen value and fire if it observed a change.  Watchers
    /// registered by callbacks during the pass are appended to the
    /// table by the registration methods and picked up by the caller's
    /// walk; their `last` starts at the current value, so they first
    /// fire on a later transition.
    fn dispatch_visit(&mut self, i: usize) {
        // Decide (and observe) with immutable access, then act, so the
        // callback call below can take `&mut self`.
        let (fire, observed) = {
            let watcher = &self.watchers[i];
            let current = self.state.peek(watcher.signal);
            if *current == watcher.last {
                (false, None)
            } else {
                let fire = match watcher.kind {
                    WatchKind::Change => true,
                    WatchKind::ClockRising => {
                        !watcher.last.get_bit(0) && current.get_bit(0)
                    }
                };
                (fire, Some(current.clone()))
            }
        };
        if let Some(value) = observed {
            self.watchers[i].last = value;
        }
        if fire {
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_watcher_fire();
            let callback = self.watchers[i].callback.clone();
            (callback.borrow_mut())(self);
            if self.sched.is_shutdown_requested() {
                self.watchers[i].alive = false;
            }
        }
    }

    /// Rebuild `watcher_buckets` from the live table: called after the
    /// shutdown retain prunes dead watchers, so the buckets keep
    /// covering exactly the table, in registration order.
    fn rebuild_buckets(&mut self) {
        self.watcher_buckets.clear();
        // The prune may have dropped a signal's last watcher: rebuild
        // the watched flags alongside the buckets, so watched-only
        // marking keeps covering exactly the signals the live table
        // observes.  The changed set is empty here (the sweep loop
        // exits only when the set is empty), so no stale bit for a
        // newly-unwatched signal survives the rebuild.
        self.state.clear_watched();
        for (index, watcher) in self.watchers.iter().enumerate() {
            let idx = watcher.signal.index();
            if self.watcher_buckets.len() <= idx {
                self.watcher_buckets.resize(idx + 1, Vec::new());
            }
            self.watcher_buckets[idx].push(index);
            self.state.set_watched(watcher.signal, true);
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
            let n = circuit.clock_sensitivity_count(signal_id);
            for i in 0..n {
                let reg_id = circuit.clock_sensitivity_at(signal_id, i);
                state.transfer(reg_id);
            }
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_transfers(n as u64);
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
            let n = circuit.clock_sensitivity_count(signal_id);
            for i in 0..n {
                let reg_id = circuit.clock_sensitivity_at(signal_id, i);
                state.transfer(reg_id);
            }
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_transfers(n as u64);
        }
        wrote
    }

    fn set_without_flow(&mut self, signal_id: SignalId, value: Value) {
        let old = self.state.get(signal_id);
        Self::transfer_clock_split(&self.circuit, &mut self.state, signal_id, &old, value);
    }

    pub fn get(&self, signal_id: SignalId) -> Value {
        self.state.get(signal_id)
    }

    /// The interned [`TypeId`] of `signal_id`'s type — for constructing
    /// `Value`s of that type without cloning the `Type`.
    pub fn component_type_id(&self, signal_id: SignalId) -> TypeId {
        self.circuit.type_layout_index.type_id(&self.elaboration().component(signal_id).typ())
    }

    pub fn now(&self) -> u64 {
        self.sched.time_ps()
    }

    /// Acquire a lock that defers circuit settlement.  While the lock is held,
    /// calls to `set()` accumulate signal changes without propagating them.
    /// When the lock is dropped, all accumulated changes are settled in a single
    /// `flow()` pass, and any watchers that fired are invoked.
    pub fn lock(&mut self) -> SimLock<'_> {
        SimLock { sim: self }
    }

    /// Request that the run loop stop dispatching after the current
    /// callback returns.  Safe to call from inside a callback; a watcher
    /// whose callback calls `finish()` is removed from the watcher table
    /// and does not survive into a subsequent `run()`.
    pub fn finish(&mut self) {
        self.sched.request_shutdown();
    }

    /// Drive `signal` as a free-running square wave described by `clock`.
    /// The signal is initialised low immediately and the first rising
    /// edge happens at `now + period_ps / 2`.
    ///
    /// The clock registers in the free-running clock table instead of
    /// as a scheduler event: the run loop advances time and toggles the
    /// signal directly whenever the next edge beats every queued user
    /// event, skipping the per-half-period heap push/pop and callback
    /// dispatch.  The edge keeps its old scheduler-event ordering
    /// against user `at`/`after` events: deadline `now + half`, FIFO
    /// sequence allocated after this settlement, exactly as the old
    /// `after(half)` registration.
    pub fn attach_clock(&mut self, signal: SignalId, clock: Clock) {
        let half = clock.period_ps / 2;
        {
            let typ_id = self.component_type_id(signal);
            let mut lock = self.lock();
            lock.set(signal, Value::from_bool(typ_id, false));
        }
        let at_ps = self.sched.time_ps().saturating_add(half);
        let seq = self.sched.alloc_seq();
        self.clocks.push(ClockHandle {
            signal,
            half_period_ps: half,
            next: Some((at_ps, seq)),
        });
    }

    /// Drain the queue.  Order:
    ///   1. All `Event::StartOfSimulation` entries fire, in FIFO order.
    ///   2. While `finish()` has not been requested, repeatedly fire
    ///      whichever comes next, free-running clock edge or
    ///      `Event::AfterDelay` entry, compared in the heap's own
    ///      `(time, seq)` order — so a tie between an edge and a user
    ///      event keeps the sequence order the edge would have had as a
    ///      heap entry.  Each callback may itself register more entries
    ///      and trigger `set()`s; value-change watchers are dispatched
    ///      by the persistent watcher table at each settlement
    ///      (`Sim::unlock`), not through the queue.  An edge that beats
    ///      every queued event is stepped directly (`Sim::step_clock`),
    ///      without touching the heap.
    ///   3. All `Event::EndOfSimulation` entries fire, in FIFO order.
    /// Watchers (`on_change`/`on_clock`) persist across `run()` calls;
    /// only a watcher whose callback called `finish()` is removed.
    /// Free-running clocks likewise persist: an edge armed at the end
    /// of one `run()` is still pending when the next one starts, as it
    /// was as a leftover heap entry.
    pub fn run(&mut self) -> Result<(), SimError> {
        // Total run-loop elapsed: the one timer that spans everything
        // below (see `sim::profile`).
        #[cfg(feature = "sim-profile")]
        let _profile_phase = crate::sim::profile::time_run();
        let start_callbacks = self.sched.drain_lifecycle(|e| matches!(e, Event::StartOfSimulation));
        for mut cb in start_callbacks {
            cb(self);
        }

        while !self.sched.is_shutdown_requested() {
            let event_next = self.sched.peek_next_timed();
            let clock_next = self.next_clock_edge();
            // The clock edge fires first when it beats the soonest
            // queued user event in the heap's own `(time, seq)` order.
            let clock_first = match (event_next, clock_next) {
                (Some((t, s)), Some((_, (edge_ps, edge_seq)))) => {
                    (edge_ps, edge_seq) < (t, s)
                }
                (None, Some(_)) => true,
                _ => false,
            };
            if clock_first {
                let (index, _) = clock_next.unwrap();
                self.step_clock(index);
            } else if let Some(mut cb) = self.sched.next_after_delay() {
                cb(self);
            } else {
                break;
            }
        }

        let end_callbacks = self.sched.drain_lifecycle(|e| matches!(e, Event::EndOfSimulation));
        for mut cb in end_callbacks {
            cb(self);
        }
        Ok(())
    }

    /// The soonest pending edge across all free-running clocks, as
    /// `(handle index, (time, seq))`, compared in the heap's `(time,
    /// seq)` order so same-time edges fire in registration order,
    /// exactly as they did as heap entries.  `None` when no clock has
    /// a pending edge (every armed handle, or none at all).
    fn next_clock_edge(&self) -> Option<(usize, (u64, u64))> {
        let mut best: Option<(usize, (u64, u64))> = None;
        for (i, handle) in self.clocks.iter().enumerate() {
            if let Some(next) = handle.next {
                best = match best {
                    None => Some((i, next)),
                    Some((_, best_next)) if next < best_next => Some((i, next)),
                    _ => best,
                };
            }
        }
        best
    }

    /// Step one free-running clock edge through exactly the work the
    /// old toggle callback did: advance `time_ps` to the edge, toggle
    /// the signal through the same commit path (`set_without_flow` ->
    /// `transfer_clock`, preserving rising-edge register transfer),
    /// settle exactly as a `SimLock` drop (`unlock`: flow plus the
    /// outermost dispatch sweep, `changed_since_dispatch` skip
    /// included), then re-arm the next edge without touching the heap
    /// or allocating.
    ///
    /// The handle is marked in-flight (no pending edge) across the
    /// settlement, mirroring the old heap state: the popped toggle
    /// entry was gone and the next one was only registered after the
    /// settlement, so a re-entrant `run()` from a settlement callback
    /// saw no pending edge for this clock.
    fn step_clock(&mut self, index: usize) {
        let (edge_ps, _seq) = self.clocks[index]
            .next
            .take()
            .expect("stepping a clock edge that is not armed");
        let signal = self.clocks[index].signal;
        let half = self.clocks[index].half_period_ps;
        self.sched.advance_to(edge_ps);
        // The old callback's toggle: read the live value, flip it,
        // commit through the lock's write path.  Toggling the live
        // value (not a stored phase) keeps the behavior when another
        // writer drove the signal between edges.
        let current = self.state.get(signal);
        let next = current.toggled();
        self.set_without_flow(signal, next);
        // The same settlement a `SimLock` drop runs.
        self.unlock();
        // Re-arm exactly as the old callback's `after(half)` did after
        // the settlement: deadline `edge + half` (a settlement cannot
        // move time), sequence allocated after any events the
        // settlement's callbacks registered.
        let at_ps = self.sched.time_ps().saturating_add(half);
        let seq = self.sched.alloc_seq();
        self.clocks[index].next = Some((at_ps, seq));
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_clock_edge();
    }

}

////////////////////////////////////////////////////////////////////////////////
// Elaboration functions
////////////////////////////////////////////////////////////////////////////////
impl Sim {
    pub(super) fn elaboration(&self) -> Arc<Elaboration> {
        self.circuit.elaboration.clone()
    }

    pub fn signal<S: AsRef<BStr>>(&mut self, path: S) -> SignalId {
        let elaboration = self.elaboration();
        elaboration.resolve(path).unwrap().id()
    }

    pub fn try_resolve<S: AsRef<BStr>>(&self, path: S) -> Option<SignalId> {
        self.elaboration().resolve(path).map(|c| c.id())
    }

    pub fn component_kind(&self, signal_id: SignalId) -> ComponentKind {
        self.elaboration().component(signal_id).component_kind()
    }

    pub fn component_type(&self, signal_id: SignalId) -> Type {
        self.elaboration().component(signal_id).typ()
    }

    pub fn full_name(&self, signal_id: SignalId) -> BString {
        self.elaboration().component(signal_id).path().clone()
    }

    /// Iterate over the direct children of `parent` (a path-prefix relation on the flat
    /// elaboration component list).  `parent_path` is the dotted path the children must
    /// extend by exactly one segment.  Yields `(id, leaf_name)` pairs.
    ///
    /// Use `children_of_root()` for the synthetic top scope (`"top"`), since the top
    /// module itself is not a component in the elaboration.
    pub fn children_of(
        &self,
        parent: SignalId,
    ) -> Vec<(SignalId, BString)> {
        let parent_path = self.elaboration().component(parent).path().clone();
        self.children_with_prefix(parent_path.as_slice())
    }

    pub fn children_of_root(&self) -> Vec<(SignalId, BString)> {
        self.children_with_prefix(b"top")
    }

    /// Every elaborated component as `(id, path)`, in elaboration
    /// order.  Unlike `children_of`, this is the exhaustive flat list:
    /// module instances are not themselves components, so a
    /// `children_of` walk cannot reach components nested inside one
    /// (e.g. `top.gcd.state`); this can.
    pub fn components(&self) -> Vec<(SignalId, BString)> {
        self.elaboration()
            .components()
            .iter()
            .map(|comp| (comp.id(), comp.path().clone()))
            .collect()
    }

    fn children_with_prefix(&self, prefix: &[u8]) -> Vec<(SignalId, BString)> {
        let elaboration = self.elaboration();
        let mut out = Vec::new();
        for comp in elaboration.components() {
            let path: &[u8] = comp.path().as_slice();
            if path.len() <= prefix.len() + 1 { continue; }
            if &path[..prefix.len()] != prefix { continue; }
            if path[prefix.len()] != b'.' { continue; }
            let rest = &path[prefix.len() + 1..];
            if rest.contains(&b'.') { continue; }
            out.push((comp.id(), BString::from(rest.to_vec())));
        }
        out
    }
}

////////////////////////////////////////////////////////////////////////////////
// Scheduler functions
////////////////////////////////////////////////////////////////////////////////
impl Sim {
    /// Register `cb` to fire at absolute simulation time `t_ps`.  If
    /// `t_ps` is in the past, the callback fires at the next dispatch
    /// step.  Same-time callbacks fire in registration (FIFO) order.
    pub fn at(&mut self, t_ps: u64, cb: Callback) {
        self.sched.register(EventCallback {
            event: Event::AfterDelay { at_ps: t_ps },
            callback: Some(cb),
        });
    }

    /// Register `cb` to fire `delay_ps` after the current simulation time.
    pub fn after(&mut self, delay_ps: u64, cb: Callback) {
        let t_ps = self.sched.time_ps().saturating_add(delay_ps);
        self.at(t_ps, cb);
    }

    /// Register `cb` to fire when `run()` begins, before any timed entries
    /// dispatch.  Multiple registrations fire in order.
    pub fn at_start(&mut self, cb: Callback) {
        self.sched.register(EventCallback {
            event: Event::StartOfSimulation,
            callback: Some(cb),
        });
    }

    /// Register `cb` to fire when `run()` is about to return — either
    /// because the queue drained or because a callback called `finish()`.
    /// Multiple registrations fire in order.
    pub fn at_end(&mut self, cb: Callback) {
        self.sched.register(EventCallback {
            event: Event::EndOfSimulation,
            callback: Some(cb),
        });
    }

    /// Register `cb` to fire every time `signal`'s value changes.
    /// Adds a persistent entry to the watcher table: no re-registration
    /// happens — the entry is swept in place at each settlement, firing
    /// (in registration order, after `flow()`) whenever the signal's
    /// value differs from the last value *this watcher* observed.
    ///
    /// If `cb` calls `sim.finish()`, the watcher is removed from the
    /// table: it does not survive into a subsequent `run()`.
    pub fn on_change(&mut self, signal: SignalId, cb: Callback) {
        let last = self.state.get(signal);
        self.watchers.push(Watcher {
            signal,
            kind: WatchKind::Change,
            last,
            alive: true,
            callback: Rc::new(RefCell::new(cb)),
        });
        let index = self.watchers.len() - 1;
        let idx = signal.index();
        if self.watcher_buckets.len() <= idx {
            self.watcher_buckets.resize(idx + 1, Vec::new());
        }
        self.watcher_buckets[idx].push(index);
        // Watched-only marking: from here on, writes to this signal pay
        // the changed-set bookkeeping (and this watcher can fire).  A
        // write before registration could not have been observed anyway:
        // `last` starts at the live value.
        self.state.set_watched(signal, true);
    }

    /// Register `cb` to fire every time `clock`'s value has a rising edge
    /// (0→1 transition on a `Bit` signal).  Adds a persistent entry to
    /// the watcher table: no re-registration happens — the entry is
    /// swept in place at each settlement, in registration order.
    ///
    /// The watcher tracks the last value *it* observed (per-watcher
    /// state, not shared circuit state), so each rising edge is observed
    /// exactly once regardless of settlement ordering.
    ///
    /// If `cb` calls `sim.finish()`, the watcher is removed from the
    /// table: it does not survive into a subsequent `run()`.
    pub fn on_clock(&mut self, clock: SignalId, cb: Callback) {
        let last = self.state.get(clock);
        self.watchers.push(Watcher {
            signal: clock,
            kind: WatchKind::ClockRising,
            last,
            alive: true,
            callback: Rc::new(RefCell::new(cb)),
        });
        let index = self.watchers.len() - 1;
        let idx = clock.index();
        if self.watcher_buckets.len() <= idx {
            self.watcher_buckets.resize(idx + 1, Vec::new());
        }
        self.watcher_buckets[idx].push(index);
        // Watched-only marking, as in `on_change`.
        self.state.set_watched(clock, true);
    }

    /// Register a callback by raw [`EventCallback`] — the uniform API
    /// behind `at`/`after`/`at_start`/`at_end`/`on_change`.  An
    /// `Event::ValueChange` entry is routed into the persistent watcher
    /// table (equivalent to `on_change`); every other event joins the
    /// scheduler queue.
    pub fn register(&mut self, event_cb: EventCallback) {
        match event_cb.event {
            Event::ValueChange { signal } => {
                if let Some(cb) = event_cb.callback {
                    self.on_change(signal, cb);
                }
            }
            _ => self.sched.register(event_cb),
        }
    }
}

/// Intentionally-empty hook for tracing register transfers when
/// debugging; params kept for call-site readability.
pub fn debug_transfer(_id: crate::analysis::elaboration::SignalId, _v: &Value) {
}

/// The dirty-set size at a settlement's start above which the
/// change-epoch skip runs (see `eval_and_commit`).  The skip's win is
/// the dedup of repeat driver visits, which requires several drivers
/// sharing inputs inside one settlement - the dirty-set size at entry
/// is the cheap predictor of that.  Below the threshold the check is
/// measured pure overhead (its skipped-eval count is exactly zero on
/// every bench design: per-edge settlements do not revisit drivers),
/// so those settlements run the plain loop, bit-for-bit the
/// pre-schedule behavior.  Measured crossover: dense-chain settlements
/// enter with hundreds of pending changes and dedup half their evals;
/// per-edge settlements enter with at most a few dozen and never skip.
/// 32 separates the two regimes with margin on both sides.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::{check_db, db_from_dir_with_lib};
    use std::cell::Cell;
    use std::path::Path;
    use std::sync::atomic::{AtomicU64, Ordering};

    /// Build a settled `Sim` for the `tests/sim/gcd` design (its source
    /// file is `top.vir`, so the ModDef symbol path is `top::Top`).
    fn gcd_sim() -> Sim {
        let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
        let source = manifest.join("../tests/sim/gcd/src");
        let lib = manifest.join("../lib");
        let db = db_from_dir_with_lib(&source, &lib);
        if let Err(diagnostics) = check_db(&db) {
            for diag in diagnostics.iter() {
                eprintln!("{diag:?}");
            }
            panic!("gcd test design failed to build");
        }
        let symboltable = db.get_symboltable();
        let top = symboltable.resolve(b"top::Top".into()).unwrap();
        Sim::new(&db, top.id(), InitStrategy::Seeded(42))
    }

    /// The sweep-skip predicate, witnessed by callback counts: a real
    /// value change fires a `Change` watcher exactly once, and a
    /// lock/unlock that writes the same value (a settlement whose
    /// `changed_since_dispatch` flag stays down, so the sweep is
    /// skipped entirely) fires nothing.
    #[test]
    fn watcher_fires_on_change_and_skips_quiescent_settlements() {
        let mut sim = gcd_sim();
        let fire = sim.signal("top.fire");
        let fire_typ = sim.component_type_id(fire);
        let initial_high = sim.get(fire).get_bit(0);

        let fires = Rc::new(RefCell::new(0u32));
        {
            let count = fires.clone();
            sim.on_change(fire, Box::new(move |_sim| {
                *count.borrow_mut() += 1;
            }));
        }

        // A real change: drive fire to the opposite of its seeded
        // initial value.
        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, !initial_high));
        }
        assert_eq!(*fires.borrow(), 1, "a real value change must fire the watcher");

        // A settlement with no actual write: the sweep is skipped, so
        // the watcher cannot fire again.
        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, !initial_high));
        }
        assert_eq!(*fires.borrow(), 1, "a same-value write must not fire the watcher");

        // A second real change fires again; repeating it does not.
        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, initial_high));
        }
        assert_eq!(*fires.borrow(), 2, "a second real change must fire again");
        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, initial_high));
        }
        assert_eq!(*fires.borrow(), 2, "a repeated same-value write must not fire");
    }

    /// `ClockRising` watchers under the skip: the first settlement
    /// drives the clock low (no fire: no change or a falling edge),
    /// the second is a real rising edge (one fire), the third writes
    /// the same high value (sweep skipped, no fire), the fourth is a
    /// falling edge (observed but no fire), the fifth is a second
    /// rising edge (one more fire).  Rising edges also transfer the
    /// gcd registers, exercising `transfer`'s flag updates.
    #[test]
    fn on_clock_watcher_fires_only_on_rising_edges() {
        let mut sim = gcd_sim();
        let clock = sim.signal("top.clock");
        let clock_typ = sim.component_type_id(clock);

        let rises = Rc::new(RefCell::new(0u32));
        {
            let count = rises.clone();
            sim.on_clock(clock, Box::new(move |_sim| {
                *count.borrow_mut() += 1;
            }));
        }

        let mut settle = |sim: &mut Sim, high: bool| {
            let mut lock = sim.lock();
            lock.set(clock, Value::from_bool(clock_typ, high));
        };

        settle(&mut sim, false);
        assert_eq!(*rises.borrow(), 0, "driving low must not fire an on_clock watcher");

        settle(&mut sim, true);
        assert_eq!(*rises.borrow(), 1, "a rising edge must fire exactly once");

        settle(&mut sim, true);
        assert_eq!(*rises.borrow(), 1, "a same-value write must not fire again");

        settle(&mut sim, false);
        assert_eq!(*rises.borrow(), 1, "a falling edge must not fire");

        settle(&mut sim, true);
        assert_eq!(*rises.borrow(), 2, "a second rising edge must fire once more");
    }

    /// A watcher callback that drives a signal during the sweep: the
    /// fire watcher fires on the settlement's change and drives `x`,
    /// whose own watcher fires within the same unlock (the dispatching
    /// loop re-sweeps after the nested flow-only unlock re-settled the
    /// driven write).  A later quiescent unlock (same fire value) fires
    /// neither: the flag was cleared by the quiescent pass and the
    /// no-op settlement skips the sweep.
    #[test]
    fn watcher_cascade_settles_within_one_unlock() {
        let mut sim = gcd_sim();
        let fire = sim.signal("top.fire");
        let x = sim.signal("top.x");
        let fire_typ = sim.component_type_id(fire);
        let x_typ = sim.component_type_id(x);
        let initial_high = sim.get(fire).get_bit(0);

        // Pre-drive x to a known value so the cascade write below is
        // guaranteed to be a real change regardless of the seeded init.
        {
            let mut lock = sim.lock();
            lock.set(x, Value::new(x_typ, BitPat::from_u64(8, 5)));
        }

        let fire_fires = Rc::new(RefCell::new(0u32));
        let x_fires = Rc::new(RefCell::new(0u32));
        {
            let count = fire_fires.clone();
            sim.on_change(fire, Box::new(move |sim| {
                *count.borrow_mut() += 1;
                let mut lock = sim.lock();
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, 6)));
            }));
        }
        {
            let count = x_fires.clone();
            sim.on_change(x, Box::new(move |_sim| {
                *count.borrow_mut() += 1;
            }));
        }

        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, !initial_high));
        }
        assert_eq!(*fire_fires.borrow(), 1, "the fire change must fire its watcher");
        assert_eq!(*x_fires.borrow(), 1, "the cascade-driven x change must fire too");

        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, !initial_high));
        }
        assert_eq!(*fire_fires.borrow(), 1, "a quiescent settlement must not re-fire");
        assert_eq!(*x_fires.borrow(), 1, "a quiescent settlement must not re-fire");
    }

    /// Bucketed dispatch keeps registration order across different
    /// signals: two watchers observing different signals, both changed
    /// by the same settlement, fire in registration order — for both
    /// registration orders, so the test pins order to registration,
    /// not to signal identity or bucket visit order.
    #[test]
    fn watchers_on_different_signals_fire_in_registration_order() {
        let run_phase = |register_fire_first: bool| -> Vec<&'static str> {
            let mut sim = gcd_sim();
            let fire = sim.signal("top.fire");
            let x = sim.signal("top.x");
            let fire_typ = sim.component_type_id(fire);
            let x_typ = sim.component_type_id(x);

            let order: Rc<RefCell<Vec<&'static str>>> = Rc::new(RefCell::new(Vec::new()));
            let fire_watcher = {
                let order = order.clone();
                Box::new(move |_sim: &mut Sim| {
                    order.borrow_mut().push("fire");
                })
            };
            let x_watcher = {
                let order = order.clone();
                Box::new(move |_sim: &mut Sim| {
                    order.borrow_mut().push("x");
                })
            };
            if register_fire_first {
                sim.on_change(fire, fire_watcher);
                sim.on_change(x, x_watcher);
            } else {
                sim.on_change(x, x_watcher);
                sim.on_change(fire, fire_watcher);
            }

            // One settlement changes both signals: a single dispatch
            // pass fires both, in registration order.
            let fire_val = sim.get(fire).get_bit(0);
            let x_val = sim.get(x).to_u64();
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !fire_val));
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_val ^ 0xFF)));
            }

            // A second settlement toggles both again: the order holds.
            let fire_val = sim.get(fire).get_bit(0);
            let x_val = sim.get(x).to_u64();
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !fire_val));
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_val ^ 0xFF)));
            }

            order.borrow().clone()
        };

        assert_eq!(
            run_phase(true),
            vec!["fire", "x", "fire", "x"],
            "fire registered first: the pass fires in registration order"
        );
        assert_eq!(
            run_phase(false),
            vec!["x", "fire", "x", "fire"],
            "x registered first: the pass fires in registration order"
        );
    }

    /// A `Change` watcher on the clock signal fires on every value
    /// change, including falling edges: the bucketed sweep visits the
    /// clock's bucket on any write to it, so the opposite-direction
    /// edge (which a `ClockRising` watcher ignores) still fires a
    /// `Change` watcher watching the same signal.
    #[test]
    fn on_change_watcher_on_clock_fires_on_falling_edges() {
        let mut sim = gcd_sim();
        let clock = sim.signal("top.clock");
        let clock_typ = sim.component_type_id(clock);

        let fires = Rc::new(RefCell::new(0u32));
        {
            let count = fires.clone();
            sim.on_change(clock, Box::new(move |_sim| {
                *count.borrow_mut() += 1;
            }));
        }

        let initial_high = sim.get(clock).get_bit(0);
        let mut settle = |sim: &mut Sim, high: bool| {
            let mut lock = sim.lock();
            lock.set(clock, Value::from_bool(clock_typ, high));
        };

        // Drive to the opposite of the seeded initial value: one
        // change, one fire (the edge direction depends on the seed).
        settle(&mut sim, !initial_high);
        assert_eq!(*fires.borrow(), 1, "the first change must fire");

        // Drive back: the opposite-direction edge (falling when the
        // initial value was high) fires a Change watcher too.
        settle(&mut sim, initial_high);
        assert_eq!(
            *fires.borrow(),
            2,
            "the opposite-direction edge must fire a Change watcher"
        );

        // A same-value write is not a change: no fire.
        settle(&mut sim, initial_high);
        assert_eq!(*fires.borrow(), 2, "a same-value write must not fire");
    }

    /// Unregister edge case: a watcher whose callback calls `finish()`
    /// is pruned from the table (and its bucket rebuilt) at the end of
    /// the unlock, while watchers on other signals survive it and keep
    /// firing.  The surviving watcher's own later fire then dies to the
    /// post-shutdown rule (watchers firing after the shutdown request
    /// do not persist), so a final settlement of its signal fires
    /// nothing.
    #[test]
    fn finish_prunes_only_the_calling_watcher_and_rebuilds_buckets() {
        let mut sim = gcd_sim();
        let fire = sim.signal("top.fire");
        let x = sim.signal("top.x");
        let fire_typ = sim.component_type_id(fire);
        let x_typ = sim.component_type_id(x);

        let x_fires = Rc::new(RefCell::new(0u32));
        {
            let count = x_fires.clone();
            sim.on_change(x, Box::new(move |_sim| {
                *count.borrow_mut() += 1;
            }));
        }
        {
            sim.on_change(fire, Box::new(move |sim| {
                sim.finish();
            }));
        }

        // The fire watcher fires, calls finish(), and is pruned; the
        // x watcher (registered earlier, on another signal) survives
        // the retain and the bucket rebuild.
        let fire_initial = sim.get(fire).get_bit(0);
        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, !fire_initial));
        }
        let x_initial = sim.get(x).to_u64();
        {
            let mut lock = sim.lock();
            lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_initial ^ 0xFF)));
        }
        assert_eq!(
            *x_fires.borrow(),
            1,
            "the surviving watcher must still fire after the finish() prune and bucket rebuild"
        );

        // The survivor's next fire runs after the shutdown request, so
        // it dies (existing post-finish semantics): no third fire.
        let x_now = sim.get(x).to_u64();
        {
            let mut lock = sim.lock();
            lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_now ^ 0xFF)));
        }
        assert_eq!(
            *x_fires.borrow(),
            1,
            "a watcher firing after the shutdown request must not persist"
        );
    }

    /// Watched-only marking must not lose fires: a watcher on `x`
    /// still fires on every one of `x`'s transitions even though each
    /// settlement also writes `fire`, an unwatched signal whose writes
    /// no longer touch the changed set.  A watched signal's write is
    /// marked regardless of what else the settlement does, so the
    /// watcher's fires are exactly the whole-table sweep's.
    #[test]
    fn watcher_fires_alongside_unwatched_signal_writes() {
        let mut sim = gcd_sim();
        let fire = sim.signal("top.fire");
        let x = sim.signal("top.x");
        let fire_typ = sim.component_type_id(fire);
        let x_typ = sim.component_type_id(x);

        let fires = Rc::new(RefCell::new(0u32));
        {
            let count = fires.clone();
            sim.on_change(x, Box::new(move |_sim| {
                *count.borrow_mut() += 1;
            }));
        }

        // Each settlement writes both signals: `fire` (unwatched) and
        // `x` (watched).  The watcher fires once per settlement, once
        // per real change of `x` — the unwatched signal's write in the
        // same settlement neither suppresses nor delays it.
        for i in 0..3u32 {
            let x_val = sim.get(x).to_u64();
            let fire_val = sim.get(fire).get_bit(0);
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !fire_val));
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_val ^ 0xFF)));
            }
            assert_eq!(*fires.borrow(), i + 1, "settlement {} must fire the x watcher once", i);
        }
    }

    /// Zero-watcher designs skip the changed-set bookkeeping entirely:
    /// with no watcher anywhere, every write leaves the changed set
    /// empty (the sweep-skip gate stays down and the dispatch loop
    /// never runs), while the circuit still settles normally.
    #[test]
    fn zero_watcher_design_leaves_the_changed_set_empty() {
        let mut sim = gcd_sim();
        let fire = sim.signal("top.fire");
        let fire_typ = sim.component_type_id(fire);

        // A settlement with no watcher anywhere: the circuit still
        // settles, and the changed set stays empty.
        let initial = sim.get(fire).get_bit(0);
        {
            let mut lock = sim.lock();
            lock.set(fire, Value::from_bool(fire_typ, !initial));
        }
        assert_eq!(sim.get(fire).get_bit(0), !initial, "the write must land");
        assert!(
            !sim.state.changed_since_dispatch(),
            "no watcher: a write must not raise the sweep-skip gate"
        );
        assert_eq!(
            sim.state.changed_signal_count(),
            0,
            "no watcher: a write must not touch the changed set"
        );
    }

    /// The bits-only commit path end to end (PERF-IDEAS item 10):
    /// narrow drivers commit through the raw compare
    /// (`transfer_clock_bits` + `State::set_val_bits`), the narrow
    /// `state` register stores raw pending bits (`State::set_reg_bits`)
    /// and transfers through the raw compare, and the dirty-list
    /// protocol sees exactly the real change events: `top.valid`'s
    /// `on_change` watcher fires once per actual value transition and
    /// never on a same-value re-commit, even though the narrow drivers
    /// are re-evaluated every cycle.
    #[test]
    fn narrow_bits_commit_path_runs_gcd_to_completion() {
        let mut sim = gcd_sim();
        let clock = sim.signal("top.clock");
        let clock_typ = sim.component_type_id(clock);
        let reset = sim.signal("top.reset");
        let reset_typ = sim.component_type_id(reset);
        let fire = sim.signal("top.fire");
        let fire_typ = sim.component_type_id(fire);
        let x = sim.signal("top.x");
        let x_typ = sim.component_type_id(x);
        let y = sim.signal("top.y");
        let y_typ = sim.component_type_id(y);
        let result = sim.signal("top.result");
        let valid = sim.signal("top.valid");

        let mut settle = |sim: &mut Sim, high: bool| {
            let mut lock = sim.lock();
            lock.set(clock, Value::from_bool(clock_typ, high));
        };

        // Drive the inputs: gcd(9, 6) = 3, with reset held for the
        // first edge so the seeded-random register state starts at
        // `Idle`.
        {
            let mut lock = sim.lock();
            lock.set(reset, Value::from_bool(reset_typ, true));
            lock.set(fire, Value::from_bool(fire_typ, true));
            lock.set(x, Value::new(x_typ, BitPat::from_u64(8, 9)));
            lock.set(y, Value::new(y_typ, BitPat::from_u64(8, 6)));
        }

        // First rising edge: reset transfers `state` to `Idle`, so
        // `result`/`valid` are 0/false from here on.
        settle(&mut sim, true);
        assert_eq!(sim.get(result).to_u64(), 0);
        assert!(!sim.get(valid).get_bit(0));

        // Deassert reset: the pending next-value recomputes to
        // `Running(9, 6)` (Idle + fire), transferred at the next edge.
        {
            let mut lock = sim.lock();
            lock.set(reset, Value::from_bool(reset_typ, false));
        }

        // Watch the alias copy of the narrow `valid` driver's output:
        // the driver is re-evaluated and re-committed every cycle, and
        // only the two real transitions (false->true at Done, true->false
        // back at Idle) may fire.
        let fires = Rc::new(RefCell::new(0u32));
        {
            let count = fires.clone();
            sim.on_change(valid, Box::new(move |_sim| {
                *count.borrow_mut() += 1;
            }));
        }

        // Rising-edge sequence for `state`: Running(9,6), Running(3,6),
        // Running(3,3), Running(0,3), Running(3,0), Done(3), Idle.  The
        // register driver re-evaluates on the clock edges, so the first
        // edge after deasserting reset still commits the `Idle` pending.
        for _ in 0..6 {
            settle(&mut sim, true);
            settle(&mut sim, false);
        }
        // Eighth rising edge after Idle: `Done(3)`.
        settle(&mut sim, true);
        assert_eq!(sim.get(result).to_u64(), 3, "gcd(9, 6) = 3");
        assert!(sim.get(valid).get_bit(0));
        assert_eq!(*fires.borrow(), 1, "the false->true transition fires once");
        settle(&mut sim, false);
        // Ninth: `Done` returns to `Idle`.
        settle(&mut sim, true);
        assert_eq!(sim.get(result).to_u64(), 0);
        assert!(!sim.get(valid).get_bit(0));
        assert_eq!(*fires.borrow(), 2, "the true->false transition fires once");
    }

    /// A `Write` sink over a shared buffer, so a test can attach the
    /// VCD dumper and inspect the bytes it produced.
    struct SharedSink(Rc<RefCell<Vec<u8>>>);

    impl std::io::Write for SharedSink {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.borrow_mut().extend_from_slice(buf);
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    /// Event interleaving between the free-running clock and user
    /// `after` events: exact (tag, time) order of every dispatched
    /// callback, captured from the pre-fast-path engine (where each
    /// clock edge was a scheduler heap entry) on the same design and
    /// seed.  Covers:
    ///
    /// - an `after` strictly between two edges (7500) fires there;
    /// - an `after` registered later (7500) for an edge's exact time
    ///   (10000) fires after that edge: the edge's heap entry was
    ///   registered at 5000, so it holds the lower seq;
    /// - an `after` registered from inside an edge's settlement (at
    ///   5000) for the next edge's exact time (15000) fires BEFORE
    ///   that edge: it holds the lower seq, because the clock re-arms
    ///   only after its settlement;
    /// - `finish()` from a rising-edge watcher stops the run cleanly
    ///   with time parked at the last edge (15000).
    #[test]
    fn clock_and_after_events_interleave_like_the_heap_engine() {
        let mut sim = gcd_sim();
        let clock = sim.signal("top.clock");
        sim.attach_clock(clock, Clock::with_period_ps(10_000));

        let log: Rc<RefCell<Vec<String>>> = Rc::new(RefCell::new(Vec::new()));
        let push = |log: &Rc<RefCell<Vec<String>>>, tag: &str, t: u64| {
            log.borrow_mut().push(format!("{tag} {t}"));
        };

        {
            let log_cb = log.clone();
            let armed = Rc::new(Cell::new(false));
            let armed_cb = armed.clone();
            sim.on_clock(clock, Box::new(move |sim| {
                push(&log_cb, "clock-rise", sim.now());
                if !armed_cb.get() {
                    armed_cb.set(true);
                    let log2 = log_cb.clone();
                    sim.after(10_000, Box::new(move |sim| {
                        push(&log2, "watcher-after", sim.now());
                    }));
                }
            }));
        }
        {
            let log_cb = log.clone();
            sim.at_start(Box::new(move |sim| {
                push(&log_cb, "start", sim.now());
            }));
        }
        {
            let log_cb = log.clone();
            sim.at_start(Box::new(move |sim| {
                let log2 = log_cb.clone();
                sim.after(7_500, Box::new(move |sim| {
                    push(&log2, "between", sim.now());
                    let log3 = log2.clone();
                    sim.after(2_600, Box::new(move |sim| {
                        push(&log3, "late", sim.now());
                    }));
                    let log3 = log2.clone();
                    sim.after(2_500, Box::new(move |sim| {
                        push(&log3, "tie-after", sim.now());
                    }));
                }));
            }));
        }
        {
            let rises: Rc<Cell<u64>> = Rc::new(Cell::new(0));
            let rises_cb = rises.clone();
            sim.on_clock(clock, Box::new(move |sim| {
                let n = rises_cb.get() + 1;
                rises_cb.set(n);
                if n >= 2 {
                    sim.finish();
                }
            }));
        }

        sim.run().unwrap();
        let log = log.borrow();
        let expected = [
            "start 0",
            "clock-rise 5000",
            "between 7500",
            "tie-after 10000",
            "late 10100",
            "watcher-after 15000",
            "clock-rise 15000",
        ];
        for (got, want) in log.iter().zip(expected.iter()) {
            assert_eq!(got, want, "dispatch order diverged from the heap engine");
        }
        assert_eq!(log.len(), expected.len(), "unexpected extra dispatches");
        assert_eq!(sim.now(), 15_000, "finish() parks time at the last edge");
    }

    /// The VCD dump of a clocked run must be byte-identical to the
    /// pre-fast-path engine's, timestamp lines included: the fast path
    /// advances `time_ps` exactly where the heap-event engine did.
    /// Golden captured from the pre-change engine on the gcd design,
    /// seed 42, 16 rising edges, period 10000 ps.
    #[test]
    fn vcd_trace_is_byte_identical_to_the_heap_engine() {
        let mut sim = gcd_sim();
        let clock = sim.signal("top.clock");
        let sink = SharedSink(Rc::new(RefCell::new(Vec::new())));
        let sink_ref = sink.0.clone();
        sim.attach_vcd(sink);
        sim.attach_clock(clock, Clock::with_period_ps(10_000));

        let rises: Rc<Cell<u64>> = Rc::new(Cell::new(0));
        {
            let rises_cb = rises.clone();
            sim.on_clock(clock, Box::new(move |sim| {
                let n = rises_cb.get() + 1;
                rises_cb.set(n);
                if n >= 16 {
                    sim.finish();
                }
            }));
        }
        sim.run().unwrap();
        assert_eq!(rises.get(), 16);

        let golden_path =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("../sim-bench/golden/gcd.vcd");
        let golden = std::fs::read(&golden_path).unwrap_or_else(|e| {
            panic!(
                "cannot read VCD golden {:?}: {e} (run the pre-change capture first)",
                golden_path
            )
        });
        assert_eq!(
            sink_ref.borrow().as_slice(),
            golden.as_slice(),
            "VCD output diverged from the heap-event engine"
        );
    }

    /// Materialize a generated chain design (the `sim-bench/gen.py`
    /// shape: `Word[32]` registers chained with `+`, every register
    /// changing on every cycle) in a fresh temp directory, and build a
    /// seeded `Sim` over it.  The per-test directory name carries a
    /// process-unique counter so parallel test threads never collide.
    fn chain_sim(stages: usize) -> Sim {
        static SEQ: AtomicU64 = AtomicU64::new(0);
        let dir = std::env::temp_dir().join(format!(
            "virdant-chain-test-{}-{}-{stages}",
            std::process::id(),
            SEQ.fetch_add(1, Ordering::Relaxed),
        ));
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();
        let mut regs = String::new();
        for i in 0..stages {
            if i == 0 {
                regs.push_str("    reg r0 : Word[32] on clock {\n        it <= it + 1w32\n    }\n");
            } else {
                regs.push_str(&format!(
                    "    reg r{i} : Word[32] on clock {{\n        it <= it + r{} + 1w32\n    }}\n",
                    i - 1
                ));
            }
        }
        let top_vir = format!(
            "//! Generated test bench: {stages}-stage register chain.\n\npub mod Top for virt::Virt {{\n    incoming clock : Clock\n\n    outgoing tx : Bit\n\n{regs}    tx := r0[0]\n}}\n"
        );
        std::fs::write(src_dir.join("top.vir"), top_vir).unwrap();
        std::fs::write(
            dir.join("virdant.toml"),
            "[project]\nname = \"chain\"\n\n[prog]\ntop = \"top::Top\"\n",
        )
        .unwrap();

        let lib = Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib");
        let db = db_from_dir_with_lib(&src_dir, &lib);
        if let Err(diagnostics) = check_db(&db) {
            for diag in diagnostics.iter() {
                eprintln!("{diag:?}");
            }
            panic!("chain test design failed to build");
        }
        let symboltable = db.get_symboltable();
        let top = symboltable.resolve(b"top::Top".into()).unwrap();
        Sim::new(&db, top.id(), InitStrategy::Seeded(42))
    }

    /// The static combinational schedule engages on acyclic designs
    /// (both a real fixture and a generated chain) and is absent only
    /// when the driver graph cannot be ordered; the epoch machinery in
    /// `flow` is live exactly then.
    #[test]
    fn static_schedule_engages_on_acyclic_designs() {
        let sim = gcd_sim();
        assert!(
            sim.program.topo_schedule().is_some(),
            "gcd's driver graph is acyclic, so the schedule must engage"
        );
        let sim = chain_sim(4);
        assert!(
            sim.program.topo_schedule().is_some(),
            "the chain's driver graph is acyclic, so the schedule must engage"
        );
    }

    /// The change-epoch skip, witnessed by the profiler counters (runs
    /// under `--features sim-profile`): in one rising-edge settlement
    /// of the 32-stage chain, every register's driver is visited twice
    /// (once for its own transfer's pop, once for its predecessor's),
    /// but the second visit's trigger epoch predates the driver's
    /// first evaluation this settlement, so exactly the repeat visits
    /// are skipped: 33 driver evals (32 register drivers + tx), 31
    /// skips.  The chain is 32 stages on purpose: the settlement must
    /// enter `flow` above `EPOCH_CHECK_MIN_DIRTY` for the check to run
    /// at all (small settlements keep the plain loop).  The falling
    /// edge visits nothing, and the next rising edge re-evaluates
    /// everything (the skip never carries across settlements), while
    /// the register values advance exactly as the chain defines.
    #[test]
    #[cfg(feature = "sim-profile")]
    fn epoch_skip_dedups_repeat_driver_visits_within_a_settlement() {
        let mut sim = chain_sim(32);
        assert!(sim.program.topo_schedule().is_some());
        let clock = sim.signal("top.clock");
        let clock_typ = sim.component_type_id(clock);
        let regs: Vec<SignalId> =
            (0..32).map(|i| sim.signal(format!("top.r{i}").as_bytes())).collect();

        let before: Vec<u64> = regs.iter().map(|&r| sim.get(r).to_u64()).collect();

        let settle = |sim: &mut Sim, high: bool| {
            let mut lock = sim.lock();
            lock.set(clock, Value::from_bool(clock_typ, high));
        };

        // Warm-up settlement (drives the clock low; may be a no-op
        // write), then measure the rising edge alone.
        settle(&mut sim, false);
        crate::sim::profile::reset();
        settle(&mut sim, true);
        let snap = crate::sim::profile::snapshot();
        assert_eq!(snap.evals_total, 33, "32 register drivers + tx, each once");
        assert_eq!(snap.epoch_skips, 31, "each driver's repeat visit is skipped");
        // Word[32] arithmetic wraps at 32 bits.
        const M32: u64 = 0xFFFF_FFFF;
        assert_eq!(sim.get(regs[0]).to_u64(), (before[0] + 1) & M32, "r0 advanced by one");
        assert_eq!(
            sim.get(regs[1]).to_u64(),
            (before[1] + before[0] + 1) & M32,
            "r1 = r1 + r0 + 1 over the pre-edge values"
        );
        assert_eq!(
            sim.get(regs[3]).to_u64(),
            (before[3] + before[2] + 1) & M32,
            "r3 = r3 + r2 + 1 over the pre-edge values"
        );

        // Falling edge: the clock's write has no AST reader, so no
        // driver is visited at all.
        crate::sim::profile::reset();
        settle(&mut sim, false);
        let snap = crate::sim::profile::snapshot();
        assert_eq!(snap.evals_total, 0, "no driver reads the clock by AST reference");
        assert_eq!(snap.epoch_skips, 0);

        // Second rising edge: the transfer re-dirties every register,
        // so every driver is re-evaluated - the skip is strictly a
        // within-settlement dedup and never carries across settlements.
        crate::sim::profile::reset();
        settle(&mut sim, true);
        let snap = crate::sim::profile::snapshot();
        assert_eq!(snap.evals_total, 33, "the next settlement re-evaluates every driver");
        assert_eq!(snap.epoch_skips, 31);
        assert_eq!(sim.get(regs[0]).to_u64(), (before[0] + 2) & M32, "r0 advanced again");
    }
}
