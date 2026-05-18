use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;

use bstr::{BStr, BString};
use indexmap::{IndexMap, IndexSet};

use crate::analysis::symbols::SymbolId;
use crate::analysis::elaboration::{SignalId, Elaboration};
use crate::common::ComponentKind;
use crate::db::Db;
use crate::sim::{Scheduler, State};
use crate::sim::circuit::Circuit;
use crate::sim::eval::{Context, Value};
use crate::sim::scheduler::{Callback, Event, EventCallback};
use crate::types::Type;

pub struct Sim {
    circuit: Circuit,
    state: State,
    sched: Scheduler,
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
    pub fn new(db: Arc<Db>, top: SymbolId) -> Sim {
        let circuit = Circuit::new(db, top);

        let mut sim = Sim {
            state: State::new(circuit.elaboration.as_ref()),
            circuit,
            sched: Scheduler::new(),
        };

        sim.flow_constants();
        sim
    }

    pub(super) fn db(&self) -> Arc<Db> {
        self.circuit.db.clone()
    }

    /// Stream a VCD trace of every dumpable signal to `writer`.  The header
    /// and an initial `$dumpvars` snapshot are written synchronously; per-
    /// signal `on_change` watchers are registered to write subsequent value
    /// changes.  A final `flush()` is registered to fire from `Sim::run`'s
    /// end-of-simulation hook.  See `vcd.rs` for the bit-layout seam.
    pub fn attach_vcd<W: Write + 'static>(&mut self, writer: W) {
        crate::sim::vcd::attach(self, writer);
    }

    /*
    pub fn dump(&self) {
        let elaboration = self.elaboration();

        for elab_component in elaboration.components() {
            let id = elab_component.id();
            let path = &self.circuit.component_paths.get(&id).unwrap();
            let value = &self.state.vals[id.index()];
            println!("{path} := {value:?}");
            if elab_component.is_reg() {
                let set_value = &self.state.set_vals[&id];
                let clock_id = elab_component.clock().unwrap();
                let clock = elaboration.component(clock_id);
                println!("{path} <= {set_value:?} on {:?}", clock.path());
            }
        }
    }
    */

    fn flow_constants(&mut self) {
        // Evaluate all components whose expression has no dependencies (empty deps set).
        // These are constant-valued nodes — their driver reads nothing from the simulation
        // state, so their value is fixed and can be determined immediately.  Collect the
        // IDs first to satisfy the borrow checker, then evaluate and set each one.
        let constant_ids: Vec<SignalId> = self
            .circuit
            .deps
            .iter()
            .filter(|(id, dep_set)| dep_set.is_empty() && self.circuit.exprs.contains_key(*id))
            .map(|(id, _)| *id)
            .collect();

        for signal_id in constant_ids {
            let expr = self.circuit.exprs[&signal_id].clone();
            let context = Context::context_for_component(&self.circuit, &self.state, signal_id);
            let value = expr.eval(&context);
            self.state.set_val(signal_id, value);
        }

        // Propagate constant values to all transitively-dependent components.
        self.flow();
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
                let expr = self.circuit.exprs.get(&update_signal_id).unwrap().clone();
                let context = Context::context_for_component(&self.circuit, &self.state, update_signal_id);
                let new_value = expr.eval(&context);
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

    /// Write `value` to `signal_id` and settle the circuit before
    /// returning (D4: synchronous set).  Concretely:
    ///   1. Store the value and mark `signal_id` dirty.
    ///   2. If this was a 0→1 transition on a clock signal, transfer every
    ///      register listed in `clock_sensitivities[signal_id]`.
    ///   3. Run `flow()` to convergence.
    ///   4. Fire value-change notifications for any watched signals whose
    ///      value differs from their last recorded snapshot.
    ///
    /// Cost note: the snapshot pass is `O(|queue|)` — each `set()` scans
    /// the entire event queue for `ValueChange` entries.  See D14 in
    /// `SIM_DESIGN.md` for the deferred watcher-index optimization.
    pub fn set(&mut self, signal_id: SignalId, value: Value) {
        let watched: Vec<SignalId> = self.sched.collect_watched_signals();
        let snapshots: IndexMap<SignalId, Value> = watched
            .into_iter()
            .map(|id| { let v = self.state.get(id); (id, v) })
            .collect();

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
        self.flow();

        let mut changed: IndexSet<SignalId> = IndexSet::new();
        for (id, prev) in snapshots {
            if self.state.get(id) != prev {
                changed.insert(id);
            }
        }
        if !changed.is_empty() {
            let callbacks = self.sched.drain_value_changes(&changed);
            for mut cb in callbacks {
                cb(self);
            }
        }
    }

    pub fn get(&self, signal_id: SignalId) -> Value {
        self.state.get(signal_id)
    }

    pub fn now(&self) -> u64 {
        self.sched.time_ps()
    }

    /// Request that the run loop stop dispatching after the current
    /// callback returns.  Safe to call from inside a callback; when
    /// called from an `on_change` closure, the self-rearming wrapper
    /// also skips re-registration so no stale `ValueChange` entry is
    /// left behind in the queue.
    pub fn finish(&mut self) {
        self.sched.request_shutdown();
    }

    /// Drive `clock` as a free-running square wave with full period
    /// `period_ps`.  The signal is initialised low immediately and the
    /// first toggle is scheduled `period_ps / 2` from now, so the first
    /// rising edge happens at `now + period_ps / 2`.
    pub fn add_clock(&mut self, clock: SignalId, period_ps: u64) {
        let half = period_ps / 2;
        self.set(clock, Value::Bit(false));
        self.register_clock_toggle(clock, half);
    }

    /// Drive `clock` as a free-running square wave at frequency
    /// `freq_hz`.  Converts to picoseconds via `1e12 / freq_hz` and
    /// forwards to `add_clock`; the resulting period is therefore
    /// truncated to the nearest picosecond.
    pub fn add_clock_hz(&mut self, clock: SignalId, freq_hz: u64) {
        let period_ps = 1_000_000_000_000u64 / freq_hz;
        self.add_clock(clock, period_ps);
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
        Self::register_clock_watcher(self, clock, cell);
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
    ) {
        let cell_for_closure = cell.clone();
        sim.sched.register(EventCallback {
            event: Event::ValueChange { signal: clock },
            callback: Some(Box::new(move |s| {
                let current = s.state.get(clock);
                let last = s.state.get_prev(clock);
                let is_rising_edge = is_rising(&last, &current);

                if is_rising_edge {
                    (cell_for_closure.borrow_mut())(s);
                }
                if !s.sched.is_shutdown_requested() {
                    Sim::register_clock_watcher(s, clock, cell_for_closure.clone());
                }
            })),
        });
    }

    /// Internal: schedule one half-period toggle for `clock`.  The callback
    /// toggles the signal and re-arms itself for `half_period_ps` later.
    fn register_clock_toggle(&mut self, clock: SignalId, half_period_ps: u64) {
        self.after(half_period_ps, Box::new(move |sim| {
            let next = match sim.state.get(clock) {
                Value::Bit(b) => Value::Bit(!b),
                Value::X(_)   => Value::Bit(true),
                other => panic!("clock signal {clock:?} has non-Bit value: {other:?}"),
            };
            sim.set(clock, next);
            sim.register_clock_toggle(clock, half_period_ps);
        }));
    }
}

/// 0→1 transition on a `Bit` signal.  Any other old/new pair (including
/// X→Bit(true)) is not a rising edge — keeps `set()`'s edge
/// detection safe on non-clock signals.
fn is_rising(old: &Value, new: &Value) -> bool {
    matches!((old, new), (Value::Bit(false), Value::Bit(true)))
}
