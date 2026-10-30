//! The `Sim` runtime: combinational propagation (`flow`), register
//! transfer on clock edges, a `SimLock` for batching writes, clock
//! attachment, and the `run` loop draining the scheduler, plus
//! elaboration lookup and callback registration helpers (`at`,
//! `after`, `on_change`, `on_clock`).

use crate::analysis::symbols::SymbolId;
use crate::analysis::elaboration::{SignalId, Elaboration};
use crate::common::{ComponentKind, Width};
use crate::db::Db;
use crate::sim::bits::BitPat;
use crate::sim::circuit::Circuit;
use crate::sim::eval::{Context, Value};
use crate::sim::layout::{TypeId, TypeLayoutIndex};
use crate::sim::scheduler::{Callback, Event, EventCallback, Scheduler};
use crate::sim::state::State;
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
}

impl InitStrategy {
    pub(super) fn value(&self, typ_id: TypeId, width: Width, rng: &mut u64) -> Value {
        match self {
            InitStrategy::Random => Value::new(typ_id, BitPat::random(width, rng)),
        }
    }
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
    state: State,
    sched: Scheduler,
    /// Persistent watcher table (`on_change`/`on_clock`): registered
    /// once, swept in place at each settlement.  No per-fire
    /// re-registration, no queue rebuild.
    watchers: Vec<Watcher>,
    /// True while the watcher dispatch loop is running.  A nested
    /// `unlock()` (a callback's own `lock()`/`set()`/`unlock()`) must
    /// only settle the circuit (`flow`), never re-sweep the table —
    /// dispatch happens exactly once per settlement, in the outermost
    /// unlock.
    dispatching: bool,
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

        let mut sim = Sim {
            state: State::new(circuit.elaboration.as_ref(), &circuit.type_layout_index, &strategy),
            circuit,
            sched: Scheduler::new(),
            watchers: Vec::new(),
            dispatching: false,
        };

        sim.apply_initials();
        sim.flow_all();
        sim
    }

    /// Set every register with an `initial` statement to its initial value
    /// (registers without one keep their `InitStrategy` value).
    /// `initial` expressions are constant, so they are evaluated in an
    /// empty context; propagation happens in the following `flow_all`.
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

        let context = Context::new(vec![], self.circuit.type_layout_index.clone());
        for (signal_id, expr) in initials {
            let value = expr.eval(&context);
            self.state.set_val(signal_id, value);
        }
        self.flow();
    }

    pub(super) fn type_layout_index(&self) -> &TypeLayoutIndex {
        &self.circuit.type_layout_index
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
                let Some(expr) = self.circuit.exprs[signal_id.index()].clone() else {
                    continue;
                };
                let context = Context::context_for_component(&self.circuit, &self.state, signal_id);
                let new_value = expr.eval(&context);
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
    }

    fn flow(&mut self) {
        while let Some(signal_id) = self.state.pop_dirty() {
            let n = self.circuit.sensitivity_count(signal_id);
            for i in 0..n {
                let update_signal_id = self.circuit.sensitivity_at(signal_id, i);
                // Aliases (socket connections `:=:`, and bare-reference
                // drivers resolved by the elaboration): the value is a
                // copy of the peer's.  Edge detection below applies to
                // alias copies too (e.g. a clock routed through a bare
                // reference or a socket connection).
                let new_value = if let Some(peer_id) = self.circuit.aliases[update_signal_id.index()] {
                    self.state.get(peer_id)
                } else {
                    let expr = self.circuit.exprs[update_signal_id.index()].clone().unwrap();
                    let context = Context::context_for_component(&self.circuit, &self.state, update_signal_id);
                    let v = expr.eval(&context);
                    v
                };
                if self.circuit.registers[update_signal_id.index()] {
                    self.state.set_reg(update_signal_id, new_value);
                } else {
                    // orchestrator (flow + edge transfers + watchers) and
                    // would recurse re-entrantly into the loop we're in.
                    let old = self.state.get(update_signal_id);
                    let rising = is_rising(&old, &new_value);
                    self.state.set_val(update_signal_id, new_value);
                    // If this signal is a clock wire that just had a
                    // rising edge (e.g. a submodule port driven by `clk`),
                    // transfer the registers it clocks — the same thing
                    // `set()` does for the top-level clock.
                    if rising {
                        let m = self.circuit.clock_sensitivity_count(update_signal_id);
                        for j in 0..m {
                            let reg_id = self.circuit.clock_sensitivity_at(update_signal_id, j);
                            self.state.transfer(reg_id);
                        }
                    }
                }
            }
        }
    }

    /// Settle the circuit after a batch of writes: propagate (`flow`),
    /// then dispatch value-change watchers.
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

        self.dispatching = true;
        // Sweep-and-fire loop: each pass fires, in registration order,
        // every watcher whose observed value differs from the last value
        // it saw.  Callbacks may drive signals (settled by their nested
        // flow-only unlocks) or register more watchers; if any callback
        // fired, sweep again until a full pass fires nothing — cascades
        // still settle within this same unlock, without nested sweeps.
        // The loop runs to quiescence even past `finish()`: watchers
        // firing after the shutdown request die instead of persisting
        // (as the old re-arm check did), but their final cascades still
        // fire.
        loop {
            if !self.dispatch_pass() {
                break;
            }
        }
        self.dispatching = false;

        // Watchers whose callback ran after `finish()` was requested do
        // not survive into a subsequent `run()` (the old one-shot queue
        // achieved this by skipping the re-register step).
        if self.sched.is_shutdown_requested() {
            self.watchers.retain(|w| w.alive);
        }
    }

    /// One sweep-and-fire pass over the watcher table, in registration
    /// order.  Each watcher compares the live value against its own
    /// last-seen value; watchers that observe a change fire (for
    /// `ClockRising`, only on a 0→1 transition of bit 0), each updating
    /// its own last-seen value as it fires.  Returns true when any
    /// callback fired (the circuit state may have changed; the caller
    /// sweeps again).
    fn dispatch_pass(&mut self) -> bool {
        let mut fired = false;
        let mut i = 0;
        while i < self.watchers.len() {
            // Decide (and observe) with immutable access, then act, so
            // the callback call below can take `&mut self`.  Watchers
            // registered by callbacks during this pass append to the
            // table and are visited here too; their `last` starts at the
            // current value, so they first fire on a later transition.
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
                fired = true;
                let callback = self.watchers[i].callback.clone();
                (callback.borrow_mut())(self);
                if self.sched.is_shutdown_requested() {
                    self.watchers[i].alive = false;
                }
            }
            i += 1;
        }
        fired
    }

    fn set_without_flow(&mut self, signal_id: SignalId, value: Value) {
        let old = self.state.get(signal_id);
        let rising = is_rising(&old, &value);
        self.state.set_val(signal_id, value);
        if rising {
            let n = self.circuit.clock_sensitivity_count(signal_id);
            for i in 0..n {
                let reg_id = self.circuit.clock_sensitivity_at(signal_id, i);
                self.state.transfer(reg_id);
            }
        }
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
    /// The signal is initialised low immediately and the first toggle is
    /// scheduled `period_ps / 2` from now, so the first rising edge
    /// happens at `now + period_ps / 2`.
    pub fn attach_clock(&mut self, signal: SignalId, clock: Clock) {
        let half = clock.period_ps / 2;
        {
            let typ_id = self.component_type_id(signal);
            let mut lock = self.lock();
            lock.set(signal, Value::from_bool(typ_id, false));
        }
        self.register_clock_toggle(signal, half);
    }

    /// Drain the queue.  Order:
    ///   1. All `Event::StartOfSimulation` entries fire, in FIFO order.
    ///   2. While the queue contains an `Event::AfterDelay` entry and
    ///      `finish()` has not been requested: pop the soonest one (FIFO
    ///      tiebreak), advance simulated time to its deadline, and fire
    ///      it.  Each callback may itself register more entries and
    ///      trigger `set()`s; value-change watchers are dispatched by
    ///      the persistent watcher table at each settlement
    ///      (`Sim::unlock`), not through the queue.
    ///   3. All `Event::EndOfSimulation` entries fire, in FIFO order.
    /// Watchers (`on_change`/`on_clock`) persist across `run()` calls;
    /// only a watcher whose callback called `finish()` is removed.
    pub fn run(&mut self) -> Result<(), SimError> {
        let start_callbacks = self.sched.drain_lifecycle(|e| matches!(e, Event::StartOfSimulation));
        for mut cb in start_callbacks {
            cb(self);
        }

        while !self.sched.is_shutdown_requested() {
            if let Some(mut cb) = self.sched.next_after_delay() {
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

    /// Bounded variant of [`Sim::run`] for incremental drivers (the
    /// wasm web demo): behaves like `run`, but stops dispatching once
    /// simulation time reaches `end_ps` (or earlier, if `finish()` is
    /// requested or the queue starves).  End-of-simulation callbacks
    /// are left queued: unlike `run`, the run may be resumed with a
    /// later `run_until` (or concluded with `run`).  Start-of-simula-
    /// tion callbacks fire on the first call only.
    ///
    /// NOTE: like `run`, a `finish()` requested inside a callback is
    /// reset by the next call's start-of-simulation drain; callers
    /// that must not resume after `finish()` should track that
    /// themselves.
    pub fn run_until(&mut self, end_ps: u64) -> Result<(), SimError> {
        let start_callbacks = self.sched.drain_lifecycle(|e| matches!(e, Event::StartOfSimulation));
        for mut cb in start_callbacks {
            cb(self);
        }

        while !self.sched.is_shutdown_requested() {
            if let Some(mut cb) = self.sched.next_after_delay_until(end_ps) {
                cb(self);
            } else {
                break;
            }
        }
        Ok(())
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

    /// Internal: schedule one half-period toggle for `clock`.  The callback
    /// toggles the signal and re-arms itself for `half_period_ps` later.
    fn register_clock_toggle(&mut self, clock: SignalId, half_period_ps: u64) {
        self.after(half_period_ps, Box::new(move |sim| {
            let current = sim.state.get(clock);
            let next = current.toggled();
            {
                let mut lock = sim.lock();
                lock.set(clock, next);
            }
            sim.register_clock_toggle(clock, half_period_ps);
        }));
    }
}

/// Intentionally-empty hook for tracing register transfers when
/// debugging; params kept for call-site readability.
pub fn debug_transfer(_id: crate::analysis::elaboration::SignalId, _v: &Value) {
}

/// 0→1 transition on a 1-bit signal.  Keeps `set()`'s edge detection
/// safe on non-clock (and multi-bit) signals.
fn is_rising(old: &Value, new: &Value) -> bool {
    old.width() == 1 && !old.get_bit(0) && new.get_bit(0)
}
