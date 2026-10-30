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
use crate::sim::eval::{Context, Value};
use crate::sim::layout::{TypeId, TypeLayoutIndex};
use crate::sim::scheduler::{Callback, Event, EventCallback, Scheduler};
use crate::sim::state::State;
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
    /// Compiled drivers: one code range per component whose driver
    /// lowered to bytecode (`None` = interpreter fallback).
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
    /// True while the watcher dispatch loop is running.  A nested
    /// `unlock()` (a callback's own `lock()`/`set()`/`unlock()`) must
    /// only settle the circuit (`flow`), never re-sweep the table —
    /// dispatch happens exactly once per settlement, in the outermost
    /// unlock.
    dispatching: bool,
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
        // Debug-build visibility into how much of the design still
        // runs on the interpreter (temporary fallback, see vm::compile).
        #[cfg(debug_assertions)]
        {
            let counts = program.fallback_counts();
            let total: usize = counts.iter().sum();
            if total > 0 {
                eprintln!(
                    "sim: {} driver(s) fell back to the interpreter (when: {}, match: {}, unsupported: {})",
                    total, counts[0], counts[1], counts[2]
                );
            }
        }
        let vm_pool = vm::Vm::new(&program);

        let mut sim = Sim {
            state: State::new(circuit.elaboration.as_ref(), &circuit.type_layout_index, &strategy),
            circuit,
            program,
            vm: vm_pool,
            sched: Scheduler::new(),
            clocks: Vec::new(),
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

    /// The per-driver execution seam: compiled bytecode when the
    /// driver lowered to the VM, the interpreter otherwise.
    ///
    /// The fallback is temporary, later units delete it driver by
    /// driver as they land the remaining lowering, so every
    /// interpreter dispatch flows through exactly this one place.
    /// Initial expressions still always run on the interpreter
    /// (construction-time only; see `apply_initials`).
    fn eval_driver(&mut self, comp: SignalId) -> Value {
        // Constant-unit skip (synthesis graft 2): a driver the
        // optimizer folded entirely to a constant commits that value
        // directly, `flow`/`flow_all` skip the run.
        if let Some(value) = self.program.const_value(comp).cloned() {
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_const_skip();
            return value;
        }
        // Sampled timing, not per-call `Instant`: a read costs ~20 ns,
        // more than some drivers, so only every 256th eval is timed
        // (see `sim::profile`).  The tick also counts the eval.
        #[cfg(feature = "sim-profile")]
        let _profile_sample = crate::sim::profile::sample_eval();
        if self.program.has_driver(comp) {
            self.vm.run(&self.program, &self.state, comp)
        } else {
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_eval_interp();
            let expr = self.circuit.exprs[comp.index()].clone().unwrap();
            let context = Context::context_for_component(&self.circuit, &self.state, comp);
            expr.eval(&context)
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
                    self.eval_driver(update_signal_id)
                };
                if self.circuit.registers[update_signal_id.index()] {
                    self.state.set_reg(update_signal_id, new_value);
                } else {
                    // orchestrator (flow + edge transfers + watchers) and
                    // would recurse re-entrantly into the loop we're in.
                    // Edge detection reads the live value *before* the
                    // write, so `old` is captured at this call site.
                    let old = self.state.get(update_signal_id);
                    self.transfer_clock(update_signal_id, &old, new_value);
                }
            }
        }
    }

    /// Settle the circuit after a batch of writes: propagate (`flow`),
    /// then dispatch value-change watchers.
    ///
    /// The sweep is skipped entirely when no stored value has changed
    /// since the last sweep that observed everything (see
    /// `State::changed_since_dispatch`): no watcher can fire over
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

        // Skip the sweep entirely when no stored value has changed
        // since the last sweep that observed everything: a watcher
        // fires only on a value change (`Change`) or a 0→1 transition
        // of bit 0 (`ClockRising`), and both require the observed
        // signal's stored value to have changed, so a settlement with
        // no actual write cannot fire anything.  The flag is raised by
        // `State`'s two write primitives (`set_val`, `transfer`) on
        // every actual write, including `set_without_flow` (via
        // `transfer_clock`) and the register transfers inside this
        // sweep's own callbacks, and cleared again below, after the
        // quiescent pass has re-observed every watcher.
        if self.state.changed_since_dispatch() {
            self.dispatching = true;
            // Sweep-and-fire loop: each pass fires, in registration
            // order, every watcher whose observed value differs from the
            // last value it saw.  Callbacks may drive signals (settled
            // by their nested flow-only unlocks) or register more
            // watchers; if any callback fired, sweep again until a full
            // pass fires nothing — cascades still settle within this
            // same unlock, without nested sweeps.  The loop runs to
            // quiescence even past `finish()`: watchers firing after the
            // shutdown request die instead of persisting (as the old
            // re-arm check did), but their final cascades still fire.
            loop {
                if !self.dispatch_pass() {
                    break;
                }
            }
            self.dispatching = false;
            // The pass that broke the loop fired nothing and brought
            // every watcher's last-seen value up to the live value, so
            // it has observed everything; the next settlement may skip
            // its sweep unless a write happens first.
            self.state.clear_changed_since_dispatch();
        }

        // Watchers whose callback ran after `finish()` was requested do
        // not survive into a subsequent `run()` (the old one-shot queue
        // achieved this by skipping the re-register step).  Skipped
        // sweeps cannot affect it: a watcher only becomes dead inside
        // `dispatch_pass`, and a skipped sweep runs no callback, so the
        // table is unchanged either way.
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
        // One `Instant` pair per sweep pass; the pass itself covers the
        // whole watcher table, so the read cost stays proportionate.
        #[cfg(feature = "sim-profile")]
        let _profile_phase = crate::sim::profile::time_dispatch();
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
                #[cfg(feature = "sim-profile")]
                crate::sim::profile::bump_watcher_fire();
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

    /// Clock-transfer commit primitive, shared by `flow`'s
    /// mid-propagation path and `set_without_flow`: detect a rising
    /// edge (`old` vs `new`), write `new` to `signal_id`'s val node,
    /// and — if the write is a rising edge on a clock wire (e.g. a
    /// submodule port driven by `clk`) — transfer the registers it
    /// clocks, the same thing `set()` does for the top-level clock.
    ///
    /// Edge detection bases stay at the call sites: each reads the live
    /// value *before* the write and hands it in as `old`.
    fn transfer_clock(&mut self, signal_id: SignalId, old: &Value, new: Value) {
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
        self.state.set_val(signal_id, new);
        if rising {
            let n = self.circuit.clock_sensitivity_count(signal_id);
            for i in 0..n {
                let reg_id = self.circuit.clock_sensitivity_at(signal_id, i);
                self.state.transfer(reg_id);
            }
            #[cfg(feature = "sim-profile")]
            crate::sim::profile::bump_transfers(n as u64);
        }
    }

    fn set_without_flow(&mut self, signal_id: SignalId, value: Value) {
        let old = self.state.get(signal_id);
        self.transfer_clock(signal_id, &old, value);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::{check_db, db_from_dir_with_lib};
    use std::cell::Cell;
    use std::path::Path;

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
}
