//! The legacy engine body: combinational propagation (`flow`),
//! register transfer on clock edges (`tick_prop`), initial-value
//! application, and the value-level primitives the `Sim` facade
//! (`sim::sim`) drives it through.  This is master's interpreter
//! simulator, renamed `LegacySim` and stripped of everything the
//! facade now owns (scheduler, watcher table, watcher dispatch,
//! free-running clocks, time).
//!
//! The engine exposes value-level primitives only (`flow`, `flow_all`,
//! `set_without_flow`, `tick_prop`, `get`, `peek`, `circuit`, the
//! changed-set/watched bookkeeping the facade's bucketed dispatch
//! reads, and the `on_structural_change`/`on_clock_attached` hooks).
//! Watcher callbacks are invoked by the facade with `&mut Sim`, so the
//! engine never re-enters dispatch.

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::analysis::symbols::SymbolId;
use crate::db::Db;
use crate::sim::circuit::Circuit;
use crate::sim::legacy::eval::Context;
use crate::sim::sim::InitStrategy;
use crate::sim::legacy::state::State;
use crate::sim::value::Value;

use std::sync::Arc;

pub(crate) struct LegacySim {
    pub(crate) circuit: Circuit,
    state: State,
}

impl LegacySim {
    pub(crate) fn new(db: &Db, top: SymbolId, strategy: &InitStrategy) -> LegacySim {
        let circuit = Circuit::new(db, top);

        let mut engine = LegacySim {
            state: State::new(
                circuit.elaboration.as_ref(),
                &circuit.type_layout_index,
                strategy,
                &circuit.memories,
            ),
            circuit,
        };

        engine.apply_initials();
        engine.flow_all();
        engine
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

        let context = crate::sim::legacy::eval::Context::new(vec![], self.circuit.type_layout_index.clone());
        for (signal_id, expr) in initials {
            let value = expr.eval(&context);
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
                // Alias-driven components (bare-reference drivers and
                // socket connections) have no expr of their own: their
                // value is a copy of the peer's.  A register in this
                // position must still get its pending staged here —
                // otherwise it would carry the random `InitStrategy`
                // pending into the first clock edge (the dirty-driven
                // `flow` only restages it when the peer's value changes,
                // which may not happen before the first edge).
                if let Some(peer_id) = self.circuit.aliases[signal_id.index()] {
                    if self.circuit.registers[signal_id.index()] {
                        self.state.set_reg(signal_id, self.state.get(peer_id));
                    }
                    continue;
                }
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
        // Construction's full-network pass has observed every value,
        // and no watcher exists yet (watchers registered later capture
        // the live value at registration time), so the first
        // settlement's sweep-skip predicate starts clean.
        self.state.clear_changed_since_dispatch();
    }

    /// The propagation-only core of a clock edge: register transfers
    /// through the sensitivity graph plus `flow`, with no watcher
    /// dispatch.  The recursion inside `tick_prop` settles inner edges
    /// silently; watchers dispatch once, in the facade's `tick`/`run`.
    fn tick_inner(&mut self, signal_id: SignalId) {
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
            self.tick_inner(inner);
        }

        self.commit_domain(signal_id);
    }

    /// Commit a rising edge of `signal_id`'s clock domain, in two
    /// phases so that no member of the domain ever sees another
    /// member's post-edge value — the nonblocking semantics of the
    /// emitted Verilog.
    ///
    /// Phase 1 evaluates every memory's write site — (enable,
    /// address, data) — against pre-edge values and stages the
    /// resulting writes (registers need no staging: `flow` already
    /// sampled their next values into the `set_vals` pendings, so
    /// their write sites ran pre-edge too).  Phase 2 then transfers
    /// the registers and applies the staged memory writes together.
    /// Committing member by member instead — the register
    /// immediately, the memory write site evaluated against live
    /// values — would let a memory declared after a register sample
    /// that register's post-edge value, making a write site's
    /// sampling depend on declaration order.
    ///
    /// Each member commits at most once per settlement (the
    /// `mark_committed` guard), so a clock alias rising again later
    /// in the same flow re-runs both phases as a no-op.
    fn commit_domain(&mut self, signal_id: SignalId) {
        let m = self.circuit.clock_sensitivity_count(signal_id);
        // Phase 1: stage memory write sites against pre-edge values.
        for i in 0..m {
            let id = self.circuit.clock_sensitivity_at(signal_id, i);
            if self.circuit.is_memory(id) {
                self.stage_memory_write(id);
            }
        }
        // Phase 2: transfer the registers, then apply the staged
        // writes.  Neither reads live values (the register copies
        // its staged pending; the write applies a sampled word), so
        // the two loops cannot interact.
        for i in 0..m {
            let id = self.circuit.clock_sensitivity_at(signal_id, i);
            if !self.circuit.is_memory(id) {
                self.state.transfer(id);
            }
        }
        for (mem_id, index, value) in self.state.drain_pending_memory_writes() {
            self.state.set_memory_word(mem_id, index, value);
        }
    }

    /// Stage a memory's write site for the current clock-edge commit:
    /// sample the (enable, address, data) expressions against
    /// pre-edge values and queue the write
    /// (`State::stage_memory_write`) without touching the words
    /// (read-during-write sees the old value because the words are
    /// only updated by phase 2; at most one write fires — overlapping
    /// guards are rejected by analysis).
    fn stage_memory_write(&mut self, mem_id: SignalId) {
        // At most one commit per settlement, like a register: a clock
        // alias rising again later in the same flow must not re-write
        // with post-edge values.
        if !self.state.mark_committed(mem_id) {
            return;
        }
        let (enable, address, data) = {
            let writes = &self.circuit.memory(mem_id).writes;
            (
                writes.enable.clone(),
                writes.address.clone(),
                writes.data.clone(),
            )
        };
        let context = Context::context_for_component(&self.circuit, &self.state, mem_id);
        if !enable.eval(&context).get_bit(0) {
            return;
        }
        let index = address.eval(&context).to_u64() as usize;
        let value = data.eval(&context);
        self.state.stage_memory_write(mem_id, index, value);
    }

    pub(crate) fn flow(&mut self) {
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
                        self.commit_domain(update_signal_id);
                    }
                }
            }
        }
        // The settlement has reached quiescence.  Reset the per-settlement
        // commit markers so the next settlement's clock edges (including
        // alias-domain edges detected mid-`flow`) commit fresh.
        self.state.clear_committed();
    }

    pub(crate) fn set_without_flow(&mut self, signal_id: SignalId, value: Value) {
        let old = self.state.get(signal_id);
        let rising = is_rising(&old, &value);
        self.state.set_val(signal_id, value);
        if rising {
            self.commit_domain(signal_id);
        }
    }

    pub(crate) fn get(&self, signal_id: SignalId) -> Value {
        self.state.get(signal_id)
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

    /// Borrow the live value of `signal_id` without cloning it (for
    /// allocation-free watcher-sweep comparisons in the facade).
    pub(crate) fn peek(&self, signal_id: SignalId) -> &Value {
        self.state.peek(signal_id)
    }

    pub(crate) fn changed_since_dispatch(&self) -> bool {
        self.state.changed_since_dispatch()
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
    /// `finish()` pruning): on the VM engine this invalidates the
    /// settlement schedule caches; the legacy interpreter has no cached
    /// schedules, so it is a no-op.
    pub(crate) fn on_structural_change(&mut self) {
    }

    /// Hook for the first clock attachment: on the VM engine this
    /// computes the whole-design cycle plan; the legacy interpreter has
    /// no cycle plan, so it is a no-op.
    pub(crate) fn on_clock_attached(&mut self, _signal: SignalId) {
    }

    /// Disassemble the compiled driver programs: VM-only, so a legacy
    /// `Sim` has no bytecode to dump.
    pub(crate) fn dump_bytecode(&self) -> String {
        panic!("dump_bytecode: the legacy engine has no compiled bytecode")
    }

    /// The whole-design cycle plan verdict: VM-only.  The legacy
    /// engine never qualifies a cycle plan, so the verdict is simply
    /// "legacy".
    pub(crate) fn cycle_plan_verdict(&self) -> String {
        "legacy".to_string()
    }

    /// The cycle plan verdict with any disqualification decoded: VM-only.
    pub(crate) fn cycle_plan_diagnostic(&self) -> String {
        "legacy".to_string()
    }

    /// The legacy engine has no cycle plan.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn cycle_plan(&self) -> Option<&'static crate::sim::vm::CyclePlanResult> {
        None
    }

    /// The legacy engine never builds a static topological schedule.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn has_topo_schedule(&self) -> bool {
        false
    }
}

/// 0→1 transition on a 1-bit signal.  Keeps `set()`'s edge detection
/// safe on non-clock (and multi-bit) signals.
fn is_rising(old: &Value, new: &Value) -> bool {
    old.width() == 1 && !old.get_bit(0) && new.get_bit(0)
}
