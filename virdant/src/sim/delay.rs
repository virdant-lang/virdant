//! Static propagation-delay model for the simulator (see
//! `plans/gates_and_prop`).
//!
//! Two pieces live here:
//!
//! * [`compute_poles`] — walked once at `Circuit` build time, it assigns
//!   every signal a **pole delay**: the longest path (in gate-delay
//!   units) from a *source* through the backwards cone of the signal's
//!   driver expression.  Sources carry their own fixed delays: a
//!   register's Q pin changes [`CLOCK_TO_Q`] after the clock edge that
//!   latches it, a user write (top-level incoming port, register poke)
//!   costs [`USER_WRITE`], and constants never change (0).  The cone
//!   then accumulates gate delays through combinational components —
//!   including through register Q pins, so a path from a clock edge is
//!   clk-to-Q plus downstream logic.
//! * [`binop_delay`] / [`unop_delay`] / [`node_delay`] — the per-node
//!   gate-delay table.  Bit-parallel operations (`And`, `Inv`, …) cost
//!   one unit regardless of width; carry chains and reduction trees
//!   grow with width.
//!
//! Delays are **cosmetic**: they offset the timestamps the VCD dumper
//! writes (`settlement time + pole × unit_ps`); the simulation itself
//! still settles at zero delay.

use std::sync::Arc;

use super::expr::{Expr, ExprPayload, Referent};
use super::layout::TypeLayoutIndex;
use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::common::{BinOp, UnOp};

/// Clock-to-Q delay: a register's Q output changes this many units
/// after the clock edge that latches it (also the delay charged to a
/// user poke of a register, which forces the same pin).
const CLOCK_TO_Q: u64 = 1;

/// The delay charged to a user write: top-level incoming ports are
/// driven only by user code (pokes, `SimLock::set`), so their changes
/// cost one unit instead of appearing instantaneously.
const USER_WRITE: u64 = 1;

/// The raw delay table for binary operators, in gate-delay units
/// (pre-`scale` — the user-facing table is `scale` of these values).
/// `w` is the (common) operand width in bits.
///
/// Hardware pictures:
///   * `And`/`Or`/`Xor` — `w` parallel 1-bit gates: depth 1.
///   * `Logical*` — a single gate on the (1-bit) operands.
///   * `Eq`/`Neq` — per-bit XNOR (1) + AND reduction tree (⌈log₂ w⌉).
///   * `Lt`/`Lte`/`Gt`/`Gte` — ripple comparator: 2 gate levels per
///     bit stage, like a carry chain.
///   * `Add`/`Sub` — ripple-carry/borrow adder: each full adder is 2
///     gate delays from carry-in to carry-out.
///   * `AddCarry`/`SubBorrow` — the carry/borrow-out emerges one gate
///     after the last sum bit.
pub(super) fn binop_delay(op: BinOp, w: u64) -> u64 {
    match op {
        BinOp::And | BinOp::Or | BinOp::Xor => 1,
        BinOp::LogicalAnd | BinOp::LogicalOr | BinOp::LogicalXor => 1,
        BinOp::Eq | BinOp::Neq => ceil_log2(w) + 1,
        BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte => 2 * w,
        BinOp::Add | BinOp::Sub => 2 * w,
        BinOp::AddCarry | BinOp::SubBorrow => 2 * w + 1,
    }
}

/// The raw delay table for unary operators, in gate-delay units
/// (pre-`scale`).  `w` is the subject width in bits.
pub(super) fn unop_delay(op: UnOp, w: u64) -> u64 {
    match op {
        UnOp::Neg => 2 * w + 1, // invert (1) into a ripple add-1 (2w)
        UnOp::Inv => 1,         // parallel inverters
        UnOp::Not => 1,         // a single gate on the (1-bit) subject
    }
}

/// ⌈log₂ n⌉ (0 for n ≤ 1).
fn ceil_log2(n: u64) -> u64 {
    if n <= 1 { 0 } else { 64 - (n - 1).leading_zeros() as u64 }
}

/// Scale a raw table entry: every gate delay is 10× the original
/// model, and every previously-zero wiring delay (direct connections,
/// bit manipulation, `word()`, padding, …) becomes 1 unit.  (Sources —
/// registers, ports, constants, user writes — are not gates and stay
/// at 0; this scaling only applies to expression nodes.)
fn scale(raw: u64) -> u64 {
    if raw == 0 { 1 } else { 10 * raw }
}

/// Intrinsic gate delay of one expression node, in gate-delay units.
/// Children's own delays are *not* included — the pole walker adds
/// them.  `w` is this node's operand width where the table needs one;
/// `arms` is the `Match` arm count; `addr_width` is the dynamic-index
/// width.
fn node_delay(payload: &ExprPayload, w: u64, arms: usize, addr_width: u64) -> u64 {
    scale(raw_node_delay(payload, w, arms, addr_width))
}

fn raw_node_delay(payload: &ExprPayload, w: u64, arms: usize, addr_width: u64) -> u64 {
    match payload {
        // Bit-parallel gates and reductions.
        ExprPayload::BinOp(b) => binop_delay(b.op, w),
        ExprPayload::UnOp(u) => unop_delay(u.op, w),
        ExprPayload::Any(_) | ExprPayload::All(_) => ceil_log2(w),

        // Muxes.
        ExprPayload::If(_) => 2, // 2:1 mux (AND-OR pair)
        ExprPayload::Match(_) => 2 * ceil_log2(arms as u64),

        // A dynamic index is a decoder (2 levels) feeding one mux
        // level per address bit.
        ExprPayload::IndexDyn(_) => 2 * addr_width,

        // Everything else is wiring: padding, truncation, constant
        // indexing/slicing, concatenation, aggregate construction.
        // (`Method`/`Fn` are `todo!()` in eval — they cannot appear in
        // an elaborated driver today — but are wired through like the
        // rest so the walker stays total.)
        ExprPayload::Reference(_)
        | ExprPayload::Paren(_)
        | ExprPayload::Index(_)
        | ExprPayload::IndexRange(_)
        | ExprPayload::Word(_)
        | ExprPayload::Zext(_)
        | ExprPayload::Sext(_)
        | ExprPayload::AsClock(_)
        | ExprPayload::Cast(_)
        | ExprPayload::Trunc(_)
        | ExprPayload::As(_)
        | ExprPayload::Ctor(_)
        | ExprPayload::Struct(_)
        | ExprPayload::Enumerant(_)
        | ExprPayload::Method(_)
        | ExprPayload::Fn(_)
        | ExprPayload::BitLit(_)
        | ExprPayload::WordLit(_)
        | ExprPayload::StrLit(_)
        | ExprPayload::Hole(_)
        | ExprPayload::Dontcare(_) => 0,
    }
}

/// Per-signal pole delays, densely indexed by `SignalId::index()`,
/// in gate-delay units.  Computed once at `Circuit` build time; the
/// runtime only reads them.
#[derive(Debug, Clone)]
pub(super) struct PoleTable {
    poles: Vec<u64>,
    /// The largest pole, and the signal that carries it (for the
    /// eager overflow check's panic message).
    pub(super) max_pole: u64,
    pub(super) max_pole_signal: Option<SignalId>,
}

impl PoleTable {
    pub(super) fn pole(&self, signal: SignalId) -> u64 {
        self.poles[signal.index()]
    }
}

/// Compute the pole table for an elaborated design.
///
/// `exprs` / `aliases` / `registers` are the dense per-signal tables
/// from `Circuit::new`; `resolved_referents` maps each component's
/// driver expression to its resolved reference targets in DFS order
/// (the same order this walk visits `Reference` nodes).
pub(super) fn compute_poles(
    elaboration: &Elaboration,
    exprs: &[Option<Arc<Expr>>],
    aliases: &[Option<SignalId>],
    registers: &[bool],
    resolved_referents: &indexmap::IndexMap<SignalId, Vec<(Referent, SignalId)>>,
) -> PoleTable {
    let n = elaboration.components().len();
    let mut walker = Walker {
        memo: vec![None; n],
        visiting: vec![false; n],
        elaboration,
        exprs,
        aliases,
        registers,
        resolved_referents,
    };

    let mut poles: Vec<u64> = Vec::with_capacity(n);
    let mut max_pole = 0u64;
    let mut max_pole_signal = None;
    for comp in elaboration.components() {
        let id = comp.id();
        let pole = walker.pole_of_signal(id);
        poles.push(pole);
        if pole > max_pole {
            max_pole = pole;
            max_pole_signal = Some(id);
        }
    }
    PoleTable { poles, max_pole, max_pole_signal }
}

struct Walker<'a> {
    memo: Vec<Option<u64>>,
    visiting: Vec<bool>,
    elaboration: &'a Elaboration,
    exprs: &'a [Option<Arc<Expr>>],
    aliases: &'a [Option<SignalId>],
    registers: &'a [bool],
    resolved_referents: &'a indexmap::IndexMap<SignalId, Vec<(Referent, SignalId)>>,
}

impl Walker<'_> {
    /// The pole delay (in units) of `signal`'s current value:
    /// the longest source-to-signal path through its driver.
    fn pole_of_signal(&mut self, signal: SignalId) -> u64 {
        let i = signal.index();
        if let Some(p) = self.memo[i] {
            return p;
        }
        if self.visiting[i] {
            panic!(
                "propagation-delay model: combinational loop through signal index {i}"
            );
        }

        // Registers are no longer free: their Q output changes
        // `CLOCK_TO_Q` after the clock edge (and a user poke of a
        // register charges the same pin).
        let pole = if self.registers[i] {
            CLOCK_TO_Q
        } else if let Some(peer) = self.aliases[i] {
            // Socket connections (`:=:`) and bare-reference drivers
            // (`x := y`): a direct connection — wiring, never free:
            // it costs 1 unit.  (Register sinks are handled above:
            // their Q is clock-to-Q delayed, not a free wire.)
            self.pole_of_signal(peer) + 1
        } else {
            match &self.exprs[i] {
                Some(expr) => {
                    self.visiting[i] = true;
                    // Owned copy: the recursion needs `&mut self`, so the
                    // referent list cannot be borrowed from it.  These are
                    // small (a driver's direct references).
                    let entries: Vec<(Referent, SignalId)> = self
                        .resolved_referents
                        .get(&signal)
                        .cloned()
                        .unwrap_or_default();
                    let p = self.pole_expr(expr, 0, &mut entries.into_iter());
                    self.visiting[i] = false;
                    p
                }
                // No driver: a top-level incoming port (user writes cost
                // [`USER_WRITE`]) — or an undriven component, which
                // never changes and gets the same treatment.  Except
                // clocks: a `Clock`-typed source is the timing
                // reference its own edges are measured from, so its
                // transitions stamp at their true time — otherwise the
                // rendered edge would shift by the same amount as the
                // clk-to-Q delay it anchors, hiding both.
                None => {
                    if matches!(
                        self.elaboration.component(signal).typ(),
                        crate::types::Type::Clock
                    ) {
                        0
                    } else {
                        USER_WRITE
                    }
                }
            }
        };
        self.memo[i] = Some(pole);
        pole
    }

    /// The pole delay of an expression node: its own gate delay plus
    /// the largest child pole.
    ///
    /// `bound` is the pole inherited by pattern-bound variables
    /// (`Referent::Location`) in the current `match` scope: the bound
    /// value is the decoded subject, available after the match's mux
    /// tree.  `entries` walks the component's resolved referents in
    /// DFS order — the same order this walk visits `Reference` nodes
    /// (see `collect_referents_inner`, which this walker mirrors;
    /// `Location` referents are absent from the list and are handled
    /// via `bound` instead).
    fn pole_expr(
        &mut self,
        expr: &Expr,
        bound: u64,
        entries: &mut std::vec::IntoIter<(Referent, SignalId)>,
    ) -> u64 {
        let payload = expr.payload();
        match payload {
            // A reference's pole is its target's pole.  `Owner` (the
            // hold-current-value stand-in) and pattern-bound locals
            // contribute no path of their own.
            ExprPayload::Reference(r) => match &r.referent {
                Referent::Component(_) => {
                    let (_, target) = entries
                        .next()
                        .expect("pole walk: resolved referents exhausted");
                    self.pole_of_signal(target)
                }
                // (`Owner` entries exist in the resolved list — they are
                // pushed by `compute_resolved_referents` — so they are
                // consumed below like Component targets.)
                Referent::Owner => {
                    let _ = entries.next();
                    0
                }
                Referent::Location(_) => bound,
            },

            // Wiring: pass the largest child pole through.
            ExprPayload::Paren(p) => self.pole_expr(&p.subject, bound, entries),
            ExprPayload::Index(i) => self.pole_expr(&i.subject, bound, entries),
            ExprPayload::IndexRange(i) => self.pole_expr(&i.subject, bound, entries),
            ExprPayload::Zext(z) => self.pole_expr(&z.subject, bound, entries),
            ExprPayload::Sext(s) => self.pole_expr(&s.subject, bound, entries),
            ExprPayload::AsClock(a) => self.pole_expr(&a.subject, bound, entries),
            ExprPayload::Cast(c) => self.pole_expr(&c.subject, bound, entries),
            ExprPayload::Trunc(t) => self.pole_expr(&t.subject, bound, entries),
            ExprPayload::As(a) => self.pole_expr(&a.subject, bound, entries),
            ExprPayload::Word(w) => {
                let best = w
                    .args
                    .iter()
                    .map(|arg| self.pole_expr(arg, bound, entries))
                    .max()
                    .unwrap_or(0);
                best
            }
            ExprPayload::Ctor(c) => c
                .args
                .iter()
                .map(|arg| self.pole_expr(arg, bound, entries))
                .max()
                .unwrap_or(0),
            ExprPayload::Struct(s) => s
                .fields
                .iter()
                .map(|(_, field)| self.pole_expr(field, bound, entries))
                .max()
                .unwrap_or(0),
            ExprPayload::Method(m) => {
                let mut best = self.pole_expr(&m.subject, bound, entries);
                for arg in &m.args {
                    best = best.max(self.pole_expr(arg, bound, entries));
                }
                best
            }
            ExprPayload::Fn(f) => {
                let mut best = self.pole_expr(&f.subject, bound, entries);
                for arg in &f.args {
                    best = best.max(self.pole_expr(arg, bound, entries));
                }
                best
            }

            // Muxes: the conditions/subject and all bodies feed the
            // output through the node's own delay.
            ExprPayload::If(w) => {
                let mut best = 0;
                for (cond, body) in &w.branches {
                    best = best.max(self.pole_expr(cond, bound, entries));
                    best = best.max(self.pole_expr(body, bound, entries));
                }
                best = best.max(self.pole_expr(&w.else_branch, bound, entries));
                node_delay(payload, 0, 0, 0) + best
            }
            ExprPayload::Match(m) => {
                let subject_pole = self.pole_expr(&m.subject, bound, entries);
                let d = node_delay(payload, 0, m.arms.len(), 0);
                // Arm bodies see pattern-bound variables that carry the
                // decoded subject's value: subject pole + mux delay.
                let inner = subject_pole + d;
                let mut best = subject_pole + d;
                for (_, body) in &m.arms {
                    best = best.max(self.pole_expr(body, inner, entries));
                }
                best
            }

            // Operators: node delay stacked on the larger operand pole.
            ExprPayload::BinOp(b) => {
                let lhs = self.pole_expr(&b.lhs, bound, entries);
                let rhs = self.pole_expr(&b.rhs, bound, entries);
                let w = TypeLayoutIndex::type_width_of(b.lhs.typ()) as u64;
                node_delay(payload, w, 0, 0) + lhs.max(rhs)
            }
            ExprPayload::UnOp(u) => {
                let subject = self.pole_expr(&u.subject, bound, entries);
                let w = TypeLayoutIndex::type_width_of(u.subject.typ()) as u64;
                node_delay(payload, w, 0, 0) + subject
            }
            ExprPayload::Any(a) => {
                let subject = self.pole_expr(&a.subject, bound, entries);
                let w = TypeLayoutIndex::type_width_of(a.subject.typ()) as u64;
                node_delay(payload, w, 0, 0) + subject
            }
            ExprPayload::All(a) => {
                let subject = self.pole_expr(&a.subject, bound, entries);
                let w = TypeLayoutIndex::type_width_of(a.subject.typ()) as u64;
                node_delay(payload, w, 0, 0) + subject
            }
            ExprPayload::IndexDyn(i) => {
                let subject = self.pole_expr(&i.subject, bound, entries);
                let index = self.pole_expr(&i.index, bound, entries);
                let addr = TypeLayoutIndex::type_width_of(i.index.typ()) as u64;
                node_delay(payload, 0, 0, addr) + subject.max(index)
            }

            // Leaves: constants.
            ExprPayload::BitLit(_)
            | ExprPayload::WordLit(_)
            | ExprPayload::StrLit(_)
            | ExprPayload::Enumerant(_)
            | ExprPayload::Hole(_)
            | ExprPayload::Dontcare(_) => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sim::payload;

    #[test]
    fn ceil_log2_edges() {
        assert_eq!(ceil_log2(0), 0);
        assert_eq!(ceil_log2(1), 0);
        assert_eq!(ceil_log2(2), 1);
        assert_eq!(ceil_log2(3), 2);
        assert_eq!(ceil_log2(4), 2);
        assert_eq!(ceil_log2(5), 3);
        assert_eq!(ceil_log2(8), 3);
        assert_eq!(ceil_log2(9), 4);
        assert_eq!(ceil_log2(64), 6);
        assert_eq!(ceil_log2(65), 7);
    }

    /// The user-facing binary-operator table: raw × scale.
    fn binop_scaled(op: BinOp, w: u64) -> u64 {
        scale(binop_delay(op, w))
    }

    /// The user-facing unary-operator table: raw × scale.
    fn unop_scaled(op: UnOp, w: u64) -> u64 {
        scale(unop_delay(op, w))
    }

    #[test]
    fn binop_table() {
        // Bit-parallel gates: 10 units at any width.
        for w in [1u64, 8, 32, 64] {
            assert_eq!(binop_scaled(BinOp::And, w), 10);
            assert_eq!(binop_scaled(BinOp::Or, w), 10);
            assert_eq!(binop_scaled(BinOp::Xor, w), 10);
            assert_eq!(binop_scaled(BinOp::LogicalAnd, w), 10);
            assert_eq!(binop_scaled(BinOp::LogicalOr, w), 10);
            assert_eq!(binop_scaled(BinOp::LogicalXor, w), 10);
        }
        // XNOR layer + reduction tree, ×10.
        assert_eq!(binop_scaled(BinOp::Eq, 1), 10);
        assert_eq!(binop_scaled(BinOp::Eq, 8), 40);
        assert_eq!(binop_scaled(BinOp::Neq, 32), 60);
        // Ripple comparators and adders: 20 per bit.
        assert_eq!(binop_scaled(BinOp::Lt, 8), 160);
        assert_eq!(binop_scaled(BinOp::Gte, 1), 20);
        assert_eq!(binop_scaled(BinOp::Add, 32), 640);
        assert_eq!(binop_scaled(BinOp::Sub, 4), 80);
        // Carry/borrow-out one gate after the last sum bit, ×10.
        assert_eq!(binop_scaled(BinOp::AddCarry, 32), 650);
        assert_eq!(binop_scaled(BinOp::SubBorrow, 8), 170);
    }

    #[test]
    fn unop_table() {
        assert_eq!(unop_scaled(UnOp::Inv, 64), 10);
        assert_eq!(unop_scaled(UnOp::Not, 64), 10);
        // Invert (10) into a ripple add-1 (20w).
        assert_eq!(unop_scaled(UnOp::Neg, 8), 170);
        assert_eq!(unop_scaled(UnOp::Neg, 1), 30);
    }

    #[test]
    fn zero_wiring_delays_become_one() {
        // Every previously-zero wiring construct costs 1 unit.
        for raw in [0u64; 1] {
            assert_eq!(scale(raw), 1);
        }
        // Direct connections (bare references) and holes are wiring.
        assert_eq!(
            node_delay(
                &ExprPayload::Reference(payload::Reference {
                    referent: Referent::Owner
                }),
                8,
                0,
                0
            ),
            1
        );
        assert_eq!(
            node_delay(&ExprPayload::Hole(payload::Hole {}), 8, 0, 0),
            1
        );
        // Reductions that would be zero at width 1 floor at 1 unit.
        assert_eq!(binop_scaled(BinOp::Eq, 1), 10); // 1 -> 10, not 0
        // any/all on 1 bit: raw 0 -> 1.
        assert_eq!(ceil_log2(1), 0);
        assert_eq!(scale(ceil_log2(1)), 1);
    }
}
