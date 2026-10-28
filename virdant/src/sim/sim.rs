//! The `Sim` runtime: combinational propagation (`flow`), register
//! transfer on clock edges, a `SimLock` for batching writes, clock
//! attachment, and the `run` loop draining the scheduler, plus
//! elaboration lookup and callback registration helpers (`at`,
//! `after`, `on_change`, `on_clock`).

use std::cell::RefCell;
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexSet;

use crate::analysis::symbols::SymbolId;
use crate::analysis::elaboration::{SignalId, Elaboration};
use crate::common::{ComponentKind, Width};
use crate::db::Db;
use crate::sim::bits::BitPat;
use crate::sim::circuit::Circuit;
use crate::sim::eval::{Context, Value};
use crate::sim::layout::TypeLayoutIndex;
use crate::sim::scheduler::{Callback, Event, EventCallback, Scheduler};
use crate::sim::state::State;
use crate::types::Type;

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
    pub(super) fn value(&self, typ: Type, width: Width, rng: &mut u64) -> Value {
        match self {
            InitStrategy::Random => Value::new(typ, BitPat::random(width, rng)),
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

    /// The precomputed type bit-layouts, for decoding typed values
    /// outside the engine (e.g. the `virt::Virt` board decoding the
    /// `exit` port's `Valid[Word[8]]`).
    pub(crate) fn type_layouts(&self) -> &TypeLayoutIndex {
        &self.circuit.type_layout_index
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

            let ids: Vec<SignalId> = self.circuit.exprs.keys().copied().collect();
            for signal_id in ids {
                let expr = self.circuit.exprs[&signal_id].clone();
                let context = Context::context_for_component(&self.circuit, &self.state, signal_id);
                let new_value = expr.eval(&context);
                if self.circuit.registers.contains(&signal_id) {
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
                .aliases
                .iter()
                .map(|(sink, source)| (*sink, *source))
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
                let new_value = if let Some(&peer_id) = self.circuit.aliases.get(&update_signal_id) {
                    self.state.get(peer_id)
                } else {
                    let expr = self.circuit.exprs.get(&update_signal_id).unwrap().clone();
                    let context = Context::context_for_component(&self.circuit, &self.state, update_signal_id);
                    let v = expr.eval(&context);
                    v
                };
                if self.circuit.registers.contains(&update_signal_id) {
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

    fn unlock(&mut self) {
        self.flow();

        // Fire any watchers that were triggered during flow()
        // We check watched signals to see which ones actually changed.
        let watched: Vec<SignalId> = self.sched.collect_watched_signals();
        let mut changed: IndexSet<SignalId> = IndexSet::new();
        for id in watched {
            let changed_ = self.state.get(id) != self.state.get_prev(id);
            if changed_ {
                changed.insert(id);
            }
        }
        if !changed.is_empty() {
            let callbacks = self.sched.drain_value_changes(&changed);
            for mut cb in callbacks {
                cb(self);
            }
            // After dispatch, sync prev_vals = vals for the signals we just
            // fired callbacks for.  Without this, a signal that was set_val'd
            // once (so prev_vals = old, vals = new) but is not subsequently
            // touched would look "changed" on every later unlock() — causing
            // spurious `on_change` re-fires triggered by unrelated signal
            // activity.
            for &id in &changed {
                self.state.commit_change(id);
            }
        }
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
    /// callback returns.  Safe to call from inside a callback; when
    /// called from an `on_change` closure, the self-rearming wrapper
    /// also skips re-registration so no stale `ValueChange` entry is
    /// left behind in the queue.
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
            let typ = self.component_type(signal);
            let mut lock = self.lock();
            lock.set(signal, Value::from_bool(typ, false));
        }
        self.register_clock_toggle(signal, half);
    }

    /// Drain the queue.  Order:
    ///   1. All `Event::StartOfSimulation` entries fire, in FIFO order.
    ///   2. While the queue contains an `Event::AfterDelay` entry and
    ///      `finish()` has not been requested: pop the soonest one (FIFO
    ///      tiebreak), advance simulated time to its deadline, and fire
    ///      it.  Each callback may itself register more entries and
    ///      trigger `set()`s, which fire matching `Event::ValueChange`
    ///      entries inline.
    ///   3. All `Event::EndOfSimulation` entries fire, in FIFO order.
    /// `Event::ValueChange` entries that remain unfired (no signal
    /// change after registration) are dropped on the next `run()`.
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
    /// Implemented as a one-shot `Event::ValueChange` entry whose closure
    /// re-registers itself after invoking `cb`, so from the user's side
    /// the callback is persistent until they stop re-arming it.
    ///
    /// If `cb` calls `sim.finish()`, the re-arm step is skipped: no fresh
    /// `Event::ValueChange` entry is queued, and the watcher does not
    /// survive into a subsequent `run()`.
    pub fn on_change(&mut self, signal: SignalId, cb: Callback) {
        let cell: Rc<RefCell<Callback>> = Rc::new(RefCell::new(cb));
        Self::register_on_change_watcher(self, signal, cell);
    }

    /// Register `cb` to fire every time `clock`'s value has a rising edge
    /// (0→1 transition on a `Bit` signal).  Implemented as a one-shot
    /// `Event::ValueChange` entry whose closure checks for rising edges and
    /// re-registers itself after invoking `cb`, so from the user's side the
    /// callback is persistent until they stop re-arming it.
    ///
    /// If `cb` calls `sim.finish()`, the re-arm step is skipped: no fresh
    /// `Event::ValueChange` entry is queued, and the clock watcher does not
    /// survive into a subsequent `run()`.
    pub fn on_clock(&mut self, clock: SignalId, cb: Callback) {
        let cell: Rc<RefCell<Callback>> = Rc::new(RefCell::new(cb));
        // The watcher tracks the last value *it* observed.  The state's
        // `prev_vals` is unreliable here: nested watcher dispatches (a
        // callback driving a signal re-enters `unlock`) can
        // `commit_change` this signal before a later-registered watcher
        // runs, erasing the very transition the late watcher must see.
        let last = Rc::new(std::cell::Cell::new(self.state.get(clock).bits().get_bit(0)));
        Self::register_clock_watcher(self, clock, cell, last);
    }

    fn register_on_change_watcher(
        sim: &mut Sim,
        signal: SignalId,
        cell: Rc<RefCell<Callback>>,
    ) {
        let cell_for_closure = cell.clone();
        sim.sched.register(EventCallback {
            event: Event::ValueChange { signal },
            callback: Some(Box::new(move |s| {
                (cell_for_closure.borrow_mut())(s);
                if !s.sched.is_shutdown_requested() {
                    Sim::register_on_change_watcher(s, signal, cell_for_closure.clone());
                }
            })),
        });
    }

    fn register_clock_watcher(
        sim: &mut Sim,
        clock: SignalId,
        cell: Rc<RefCell<Callback>>,
        last: Rc<std::cell::Cell<bool>>,
    ) {
        let cell_for_closure = cell.clone();
        let last_for_closure = last.clone();
        sim.sched.register(EventCallback {
            event: Event::ValueChange { signal: clock },
            callback: Some(Box::new(move |s| {
                let current = s.state.get(clock).bits().get_bit(0);
                let last_bit = last_for_closure.get();
                let is_rising_edge = !last_bit && current;
                // Update our own last-seen value on EVERY dispatch, so
                // nested re-fires see the current state, not a stale one.
                last_for_closure.set(current);

                if is_rising_edge {
                    (cell_for_closure.borrow_mut())(s);
                }
                if !s.sched.is_shutdown_requested() {
                    Sim::register_clock_watcher(s, clock, cell_for_closure.clone(), last_for_closure.clone());
                }
            })),
        });
    }

    /// Internal: schedule one half-period toggle for `clock`.  The callback
    /// toggles the signal and re-arms itself for `half_period_ps` later.
    fn register_clock_toggle(&mut self, clock: SignalId, half_period_ps: u64) {
        self.after(half_period_ps, Box::new(move |sim| {
            let current = sim.state.get(clock);
            let next = Value::from_bool(current.typ().clone(), !current.bits().get_bit(0));
            {
                let mut lock = sim.lock();
                lock.set(clock, next);
            }
            sim.register_clock_toggle(clock, half_period_ps);
        }));
    }
}

pub fn debug_transfer(id: crate::analysis::elaboration::SignalId, v: &Value) {
}

/// 0→1 transition on a 1-bit signal.  Keeps `set()`'s edge detection
/// safe on non-clock (and multi-bit) signals.
fn is_rising(old: &Value, new: &Value) -> bool {
    old.width() == 1 && !old.bits().get_bit(0) && new.bits().get_bit(0)
}
