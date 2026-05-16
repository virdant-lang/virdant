use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use bstr::{BStr, BString};
use indexmap::{IndexMap, IndexSet};

use crate::analysis::symbols::SymbolId;
use crate::analysis::elaboration::{ElaboratedComponentId, Elaboration};
use crate::common::ComponentKind;
use crate::db::Db;
use crate::sim::circuit::Circuit;
use crate::sim::eval::{Context, Value};
use crate::sim::expr::Expr;
use crate::types::Type;


#[derive(Eq, Hash, PartialEq, Debug)]
pub struct Node {
    component_id: ElaboratedComponentId,
    is_reg_set: bool,
}

/// Live circuit values and the propagation worklist.  Everything
/// mutated by combinational propagation, register transfers, and
/// direct `set` writes lives here.
struct State {
    values: IndexMap<Node, Value>,
    dirty: IndexSet<ElaboratedComponentId>,
}

/// Event scheduler and dispatch state.  Owns simulated time, the
/// unified one-shot callback queue, the FIFO tie-breaker counter,
/// and the early-termination flag toggled by `Sim::finish`.  Every
/// registration — timed, lifecycle, or value-change — pushes one
/// entry into `queue`; every fire removes one.  See `Event` for the
/// trigger conditions.
struct Scheduler {
    time_ps: u64,
    next_seq: u64,
    shutdown_requested: bool,
    queue: Vec<QueueEntry>,
}

pub struct Sim {
    circuit: Circuit,
    state: State,
    sched: Scheduler,
    clock_last_values: IndexMap<ElaboratedComponentId, Value>,
}

/// A user-supplied callback.  Receives `&mut Sim` so it can read signals,
/// drive new values, register more callbacks, or call `finish()`.
pub type Callback = Box<dyn FnMut(&mut Sim) + 'static>;

/// Trigger condition for a queued callback.  Mirrors a subset of the VPI
/// callback reasons (see `VPI_CALLBACKS.md`); the run loop and the
/// callback API agree on this vocabulary so users always know *why* their
/// callback is being invoked.
#[derive(Debug, Clone)]
pub enum Event {
    /// Fire once when `run()` begins, before any timed entries dispatch.
    /// Counterpart of VPI `cbStartOfSimulation` (#11).
    StartOfSimulation,
    /// Fire once when `run()` is about to return (queue drained or
    /// `finish()` called).  Counterpart of VPI `cbEndOfSimulation` (#12).
    EndOfSimulation,
    /// Fire at absolute simulation time `at_ps`.  Counterpart of VPI
    /// `cbAfterDelay` (#9) / `cbAtStartOfSimTime` (#5).
    AfterDelay { at_ps: u64 },
    /// Fire when `signal`'s value changes.  Counterpart of VPI
    /// `cbValueChange` (#1).
    ValueChange { signal: ElaboratedComponentId },
}

/// One queueable (event, callback) pair.  Public so users can build
/// entries directly via `sim.register(...)` if they prefer the uniform
/// API to the convenience methods (`at`, `after`, `at_start`, …).  The
/// callback is `Option`-wrapped so the dispatcher can `take` it out and
/// invoke it with `&mut Sim` without holding a borrow into the entry.
pub struct EventCallback {
    pub event: Event,
    pub callback: Option<Callback>,
}

/// Internal queue slot: an `EventCallback` plus a monotonic sequence
/// number used for FIFO tie-breaking among same-trigger entries.
struct QueueEntry {
    seq: u64,
    cb: EventCallback,
}

#[derive(Debug)]
pub enum SimError {}

impl std::fmt::Debug for Sim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Sim")
            .field("time_ps", &self.sched.time_ps)
            .field("values", &self.state.values)
            .field("queue_len", &self.sched.queue.len())
            .finish_non_exhaustive()
    }
}

impl Sim {
    pub fn new(db: Arc<Db>, top: SymbolId) -> Sim {
        let circuit = Circuit::new(db, top);

        let mut values: IndexMap<Node, Value> = IndexMap::new();
        for elab_component in circuit.elaboration.components() {
            let elab_component_id = elab_component.id();
            let typ = elab_component.typ();

            let node = Node::val(elab_component_id);
            let value = Value::X(typ.clone());
            values.insert(node, value);

            if elab_component.is_reg() {
                let node = Node::set(elab_component_id);
                let value = Value::X(typ);
                values.insert(node, value);
            }
        }

        let mut sim = Sim {
            circuit,
            state: State {
                values,
                dirty: IndexSet::new(),
            },
            sched: Scheduler {
                time_ps: 0,
                next_seq: 0,
                shutdown_requested: false,
                queue: Vec::new(),
            },
            clock_last_values: IndexMap::new(),
        };

        // Evaluate all components whose expression has no dependencies (empty deps set).
        // These are constant-valued nodes — their driver reads nothing from the simulation
        // state, so their value is fixed and can be determined immediately.  Collect the
        // IDs first to satisfy the borrow checker, then evaluate and set each one.
        let constant_ids: Vec<ElaboratedComponentId> = sim
            .circuit
            .deps
            .iter()
            .filter(|(id, dep_set)| dep_set.is_empty() && sim.circuit.exprs.contains_key(*id))
            .map(|(id, _)| *id)
            .collect();

        for id in constant_ids {
            let expr = sim.circuit.exprs[&id].clone();
            let context = sim.build_context(id, &expr);
            let value = expr.eval(context);
            sim.write_value(id, value);
        }

        // Propagate constant values to all transitively-dependent components.
        sim.flow();

        sim
    }

    fn elaboration(&self) -> Arc<Elaboration> {
        self.circuit.elaboration.clone()
    }

    pub fn dump(&self) {
        let elaboration = self.elaboration();

        for (node, value) in &self.state.values {
            let path = &self.circuit.component_paths.get(&node.component_id).unwrap();
            if node.is_reg_set {
                let clock_id = elaboration.component(node.component_id).clock().unwrap();
                let clock = elaboration.component(clock_id);
                println!("{path} <= {value:?} on {:?}", clock.path());
            } else {
                println!("{path} := {value:?}");
            }
        }
    }

    pub fn tick(&mut self, component_id: ElaboratedComponentId) {
        self.tick_prop(component_id);
        self.flow();
    }

    fn tick_prop(&mut self, component_id: ElaboratedComponentId) {
        if let Some(sensitivities) = self.circuit.sensitivities.get(&component_id) {
            for component_id in sensitivities.clone() {
                self.tick(component_id);
            }
        }

        if let Some(clock_sensitivities) = self.circuit.clock_sensitivities.get(&component_id) {
            let clock_sensitivities = clock_sensitivities.clone();
            for reg_component_id in clock_sensitivities {
                self.transfer(reg_component_id);
            }
        }
    }

    pub fn flow(&mut self) {
        while let Some(component_id) = self.state.dirty.pop() {
            if let Some(sensitivities) = self.circuit.sensitivities.get(&component_id) {
                for update_component_id in sensitivities.clone() {
                    let expr = self.circuit.exprs.get(&update_component_id).unwrap().clone();
                    let context = self.build_context(update_component_id, &expr);
                    let new_value = expr.eval(context);
                    if self.circuit.registers.contains(&update_component_id) {
                        self.set_reg(update_component_id, new_value);
                    } else {
                        // Use write_value, not set: `set` is the user-facing
                        // orchestrator (flow + edge transfers + watchers) and
                        // would recurse re-entrantly into the loop we're in.
                        let old = self.get(update_component_id);
                        let rising = is_rising(&old, &new_value);
                        self.write_value(update_component_id, new_value);
                        // If this component is a clock wire that just had a
                        // rising edge (e.g. a submodule port driven by `clk`),
                        // transfer the registers it clocks — the same thing
                        // `set()` does for the top-level clock.
                        if rising {
                            if let Some(regs) = self.circuit.clock_sensitivities.get(&update_component_id).cloned() {
                                for reg_id in regs {
                                    self.transfer(reg_id);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn resolve<S: AsRef<BStr>>(&mut self, path: S) -> ElaboratedComponentId {
        let elaboration = self.elaboration();
        elaboration.resolve(path).unwrap().id()
    }

    pub fn try_resolve<S: AsRef<BStr>>(&self, path: S) -> Option<ElaboratedComponentId> {
        self.elaboration().resolve(path).map(|c| c.id())
    }

    pub fn component_kind(&self, component_id: ElaboratedComponentId) -> ComponentKind {
        self.elaboration().component(component_id).component_kind()
    }

    pub fn component_type(&self, component_id: ElaboratedComponentId) -> Type {
        self.elaboration().component(component_id).typ()
    }

    pub fn full_name(&self, component_id: ElaboratedComponentId) -> BString {
        self.elaboration().component(component_id).path().clone()
    }

    /// Iterate over the direct children of `parent` (a path-prefix relation on the flat
    /// elaboration component list).  `parent_path` is the dotted path the children must
    /// extend by exactly one segment.  Yields `(id, leaf_name)` pairs.
    ///
    /// Use `children_of_root()` for the synthetic top scope (`"top"`), since the top
    /// module itself is not a component in the elaboration.
    pub fn children_of(
        &self,
        parent: ElaboratedComponentId,
    ) -> Vec<(ElaboratedComponentId, BString)> {
        let parent_path = self.elaboration().component(parent).path().clone();
        self.children_with_prefix(parent_path.as_slice())
    }

    pub fn children_of_root(&self) -> Vec<(ElaboratedComponentId, BString)> {
        self.children_with_prefix(b"top")
    }

    fn children_with_prefix(&self, prefix: &[u8]) -> Vec<(ElaboratedComponentId, BString)> {
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

    /// Build an eval `Context` for `component_id`'s driver expression.
    ///
    /// `Circuit::resolve_expr_referents` does the static work \u2014 mapping
    /// each `Referent::Component` in `expr` to the `ElaboratedComponentId`
    /// it names from `component_id`'s scope.  This method zips each
    /// resolved id with the live `Value` from `self.state.values`.
    fn build_context(&self, component_id: ElaboratedComponentId, expr: &Expr) -> Context {
        let entries = self.circuit
            .resolve_expr_referents(component_id, expr)
            .into_iter()
            .map(|(r, id)| (r, self.state.values[&Node::val(id)].clone()))
            .collect();
        Context::new(entries)
    }

    /// Write `value` to `component_id` and settle the circuit before
    /// returning (D4: synchronous set).  Concretely:
    ///   1. Store the value and mark `component_id` dirty.
    ///   2. If this was a 0→1 transition on a clock signal, transfer every
    ///      register listed in `clock_sensitivities[component_id]`.
    ///   3. Run `flow()` to convergence.
    ///   4. Fire value-change notifications for any watched signals whose
    ///      value differs from their last recorded snapshot.
    ///
    /// Cost note: the snapshot pass is `O(|queue|)` — each `set()` scans
    /// the entire event queue for `ValueChange` entries.  See D14 in
    /// `SIM_DESIGN.md` for the deferred watcher-index optimization.
    pub fn set(&mut self, component_id: ElaboratedComponentId, value: Value) {
        // Snapshot every signal that has a pending Event::ValueChange entry
        // so we can detect transitions after the write + flow settles.
        let watched: Vec<ElaboratedComponentId> = self.sched.queue.iter()
            .filter_map(|q| match q.cb.event {
                Event::ValueChange { signal } => Some(signal),
                _ => None,
            })
            .collect();
        let snapshots: IndexMap<ElaboratedComponentId, Value> = watched
            .into_iter()
            .map(|id| { let v = self.get(id); (id, v) })
            .collect();

        let old = self.get(component_id);
        let rising = is_rising(&old, &value);
        self.write_value(component_id, value);
        if rising {
            if let Some(regs) = self.circuit.clock_sensitivities.get(&component_id).cloned() {
                for reg_id in regs {
                    self.transfer(reg_id);
                }
            }
        }
        self.flow();

        let mut changed: IndexSet<ElaboratedComponentId> = IndexSet::new();
        for (id, prev) in snapshots {
            if self.get(id) != prev {
                changed.insert(id);
            }
        }
        if !changed.is_empty() {
            self.fire_value_changes(&changed);
        }
    }

    /// Internal write: store `value` at `component_id`'s val node and mark
    /// it dirty.  Does not flow, does not fire watchers, does not handle
    /// clock edges.  Called by `set` (the orchestrator) and by `flow`
    /// (during propagation, to avoid re-entrant orchestration).
    fn write_value(&mut self, component_id: ElaboratedComponentId, value: Value) {
        let node = Node::val(component_id);
        let value_ref = self.state.values.get_mut(&node).unwrap();
        *value_ref = value;
        self.state.dirty.insert(component_id);
    }

    fn set_reg(&mut self, component_id: ElaboratedComponentId, value: Value) {
        let node = Node::set(component_id);
        let value_ref = self.state.values.get_mut(&node).unwrap();
        *value_ref = value;
    }

    fn transfer(&mut self, component_id: ElaboratedComponentId) {
        let new_value = self.state.values.get_mut(&Node::set(component_id)).unwrap().clone();
        let value_ref = self.state.values.get_mut(&Node::val(component_id)).unwrap();
        *value_ref = new_value;
        self.state.dirty.insert(component_id);
    }

    pub fn get(&self, component_id: ElaboratedComponentId) -> Value {
        self.state.values.get(&Node::val(component_id)).unwrap().clone()
    }

    /// Current simulation time, in picoseconds.
    pub fn now(&self) -> u64 {
        self.sched.time_ps
    }

    /// Push `event_cb` onto the queue.  The single primitive on which
    /// `at`, `after`, `at_start`, `at_end`, and `on_change` are built;
    /// expose `Event` and `EventCallback` directly when you want to skip
    /// the sugar.  Same-trigger entries fire in registration (FIFO) order.
    pub fn register(&mut self, event_cb: EventCallback) {
        let seq = self.sched.next_seq;
        self.sched.next_seq += 1;
        self.sched.queue.push(QueueEntry { seq, cb: event_cb });
    }

    /// Register `cb` to fire at absolute simulation time `t_ps`.  If
    /// `t_ps` is in the past, the callback fires at the next dispatch
    /// step.  Same-time callbacks fire in registration (FIFO) order.
    pub fn at(&mut self, t_ps: u64, cb: Callback) {
        self.register(EventCallback {
            event: Event::AfterDelay { at_ps: t_ps },
            callback: Some(cb),
        });
    }

    /// Register `cb` to fire `delay_ps` after the current simulation time.
    pub fn after(&mut self, delay_ps: u64, cb: Callback) {
        let t_ps = self.sched.time_ps.saturating_add(delay_ps);
        self.at(t_ps, cb);
    }

    /// Register `cb` to fire when `run()` begins, before any timed entries
    /// dispatch.  Multiple registrations fire in order.
    pub fn at_start(&mut self, cb: Callback) {
        self.register(EventCallback {
            event: Event::StartOfSimulation,
            callback: Some(cb),
        });
    }

    /// Register `cb` to fire when `run()` is about to return — either
    /// because the queue drained or because a callback called `finish()`.
    /// Multiple registrations fire in order.
    pub fn at_end(&mut self, cb: Callback) {
        self.register(EventCallback {
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
    pub fn on_change(&mut self, signal: ElaboratedComponentId, cb: Callback) {
        let cell: Rc<RefCell<Callback>> = Rc::new(RefCell::new(cb));
        Self::arm_value_change(self, signal, cell);
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
    pub fn on_clock(&mut self, clock: ElaboratedComponentId, cb: Callback) {
        let cell: Rc<RefCell<Callback>> = Rc::new(RefCell::new(cb));
        Self::arm_clock(self, clock, cell);
    }

    fn arm_value_change(
        sim: &mut Sim,
        signal: ElaboratedComponentId,
        cell: Rc<RefCell<Callback>>,
    ) {
        let cell_for_closure = cell.clone();
        sim.register(EventCallback {
            event: Event::ValueChange { signal },
            callback: Some(Box::new(move |s| {
                (cell_for_closure.borrow_mut())(s);
                if !s.sched.shutdown_requested {
                    Sim::arm_value_change(s, signal, cell_for_closure.clone());
                }
            })),
        });
    }

    fn arm_clock(
        sim: &mut Sim,
        clock: ElaboratedComponentId,
        cell: Rc<RefCell<Callback>>,
    ) {
        let cell_for_closure = cell.clone();
        // Store the current value as the "last" value for this clock
        let current = sim.get(clock);
        sim.clock_last_values.insert(clock, current);

        sim.register(EventCallback {
            event: Event::ValueChange { signal: clock },
            callback: Some(Box::new(move |s| {
                let current = s.get(clock);
                // Get the last value we saw for this clock
                let last = s.clock_last_values.get(&clock).cloned();

                // Only fire on rising edges: 0→1 or X→1
                let is_rising_edge = if let Some(last_val) = last {
                    is_rising(&last_val, &current)
                } else {
                    false
                };

                // Update the last seen value
                s.clock_last_values.insert(clock, current);

                if is_rising_edge {
                    (cell_for_closure.borrow_mut())(s);
                }
                if !s.sched.shutdown_requested {
                    Sim::arm_clock(s, clock, cell_for_closure.clone());
                }
            })),
        });
    }

    /// Request that the run loop stop dispatching after the current
    /// callback returns.  Safe to call from inside a callback; when
    /// called from an `on_change` closure, the self-rearming wrapper
    /// also skips re-registration so no stale `ValueChange` entry is
    /// left behind in the queue.
    pub fn finish(&mut self) {
        self.sched.shutdown_requested = true;
    }

    /// Drive `clock` as a free-running square wave with full period
    /// `period_ps`.  The signal is initialised low immediately and the
    /// first toggle is scheduled `period_ps / 2` from now, so the first
    /// rising edge happens at `now + period_ps / 2`.
    pub fn add_clock(&mut self, clock: ElaboratedComponentId, period_ps: u64) {
        let half = period_ps / 2;
        self.set(clock, Value::Bit(false));
        self.schedule_clock_toggle(clock, half);
    }

    /// Internal: schedule one half-period toggle for `clock`.  The callback
    /// toggles the signal and re-arms itself for `half_period_ps` later.
    fn schedule_clock_toggle(&mut self, clock: ElaboratedComponentId, half_period_ps: u64) {
        self.after(half_period_ps, Box::new(move |sim| {
            let next = match sim.get(clock) {
                Value::Bit(b) => Value::Bit(!b),
                Value::X(_)   => Value::Bit(true),
                other => panic!("clock signal {clock:?} has non-Bit value: {other:?}"),
            };
            sim.set(clock, next);
            sim.schedule_clock_toggle(clock, half_period_ps);
        }));
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
        self.sched.shutdown_requested = false;
        self.fire_lifecycle(|e| matches!(e, Event::StartOfSimulation));

        while !self.sched.shutdown_requested {
            let next = self.sched.queue
                .iter()
                .enumerate()
                .filter_map(|(i, q)| match q.cb.event {
                    Event::AfterDelay { at_ps } => Some((i, at_ps, q.seq)),
                    _ => None,
                })
                .min_by(|a, b| a.1.cmp(&b.1).then(a.2.cmp(&b.2)))
                .map(|(i, _, _)| i);
            let Some(idx) = next else { break };

            let mut entry = self.sched.queue.remove(idx);
            if let Event::AfterDelay { at_ps } = entry.cb.event {
                self.sched.time_ps = at_ps;
            }
            if let Some(mut cb) = entry.cb.callback.take() {
                cb(self);
            }
        }

        self.fire_lifecycle(|e| matches!(e, Event::EndOfSimulation));
        Ok(())
    }

    /// Fire and remove every queue entry whose event matches `pred`, in
    /// registration (FIFO) order.  Snapshots the matching set up-front so
    /// callbacks that re-register during dispatch don't fire in the same
    /// pass.  Used for `StartOfSimulation` / `EndOfSimulation`.
    fn fire_lifecycle<F: Fn(&Event) -> bool>(&mut self, pred: F) {
        let mut to_fire: Vec<QueueEntry> = Vec::new();
        let mut i = 0;
        while i < self.sched.queue.len() {
            if pred(&self.sched.queue[i].cb.event) {
                to_fire.push(self.sched.queue.remove(i));
            } else {
                i += 1;
            }
        }
        to_fire.sort_by_key(|q| q.seq);
        for mut entry in to_fire {
            if let Some(mut cb) = entry.cb.callback.take() {
                cb(self);
            }
        }
    }

    /// Fire and remove every `Event::ValueChange { signal }` entry whose
    /// signal is in `changed`.  Snapshots the matching set up-front so
    /// `on_change`'s self-rearming wrapper, which queues a fresh
    /// `ValueChange` after invoking the user closure, doesn't re-fire in
    /// the same pass; the new entry waits for the next change.
    fn fire_value_changes(&mut self, changed: &IndexSet<ElaboratedComponentId>) {
        let mut to_fire: Vec<QueueEntry> = Vec::new();
        let mut i = 0;
        while i < self.sched.queue.len() {
            let matches = match self.sched.queue[i].cb.event {
                Event::ValueChange { signal } => changed.contains(&signal),
                _ => false,
            };
            if matches {
                to_fire.push(self.sched.queue.remove(i));
            } else {
                i += 1;
            }
        }
        to_fire.sort_by_key(|q| q.seq);
        for mut entry in to_fire {
            if let Some(mut cb) = entry.cb.callback.take() {
                cb(self);
            }
        }
    }
}

/// 0→1 transition on a `Bit` signal.  Any other old/new pair (including
/// X→Bit(true)) is not a rising edge — keeps `set()`'s edge
/// detection safe on non-clock signals.
fn is_rising(old: &Value, new: &Value) -> bool {
    matches!((old, new), (Value::Bit(false), Value::Bit(true)))
}

impl Node {
    fn val(elab_component_id: ElaboratedComponentId) -> Node {
        Node {
            component_id: elab_component_id,
            is_reg_set: false,
        }
    }

    fn set(elab_component_id: ElaboratedComponentId) -> Node {
        Node {
            component_id: elab_component_id,
            is_reg_set: true,
        }
    }
}
