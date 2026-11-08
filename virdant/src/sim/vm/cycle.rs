//! The load-time whole-design compiler, phase 1: the qualification
//! checker and the static cycle schedule (`CyclePlan`).
//!
//! A design can be compiled to a static whole-cycle program (one
//! straight-line pass over every driver, one register-transfer point,
//! the combinational tail) only when its transfer timing is a compile-
//! time property:
//!
//! 1. *Single-clock discipline*: every register is clocked by the
//!    primary free-running clock signal (the one `attach_clock`
//!    attaches).  A register clocked by any combinational or internal
//!    signal transfers at a runtime event, not at a schedule point,
//!    so the design is disqualified (its committed register values
//!    would change under a static schedule).
//! 2. *Acyclic combinational graph*: the stage-A `TopoSchedule` must
//!    exist.  A cyclic graph refuses the schedule and falls back to
//!    the dynamic worklist, which reaches a fixpoint on any graph.
//! 3. *Translatable drivers*: every driver's optimized bytecode
//!    contains only instructions the JIT translator's narrow subset
//!    handles.  A driver outside that subset is untranslatable and
//!    disqualifies the design for now.
//! 4. *No dynamic-only constructs in the cycle*: `finish()` calls and
//!    external `set_val` writes are run-loop concerns, not schedule
//!    concerns; a later phase's fallback contract handles them.  This
//!    phase notes nothing and checks nothing for them.
//!
//! The verdict is computed once (at the first `attach_clock`, when the
//! primary clock becomes known) and stored on `Sim` unused: the engine
//! keeps its dynamic worklist.  A later phase consumes the plan.

use std::collections::VecDeque;

use crate::analysis::elaboration::SignalId;
use crate::sim::circuit::Circuit;

use super::isa::{Dst, Ins, Program};
use super::schedule::TopoSchedule;

/// Why a design does not qualify for a static whole-cycle program.
/// Every variant names the offending signal path precisely enough to
/// diagnose the design.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Disqualification {
    /// A register's clock is not the primary clock.  `register` names
    /// the offending register, `clock` the signal actually clocking
    /// it, `primary` the primary clock signal the design must use.
    RegisterClockedByNonPrimary {
        register: SignalId,
        clock: SignalId,
        primary: SignalId,
    },
    /// The combinational driver graph has a cycle (stage A's
    /// `TopoSchedule` refused the design), so the dynamic worklist is
    /// the only path that reaches a fixpoint.
    CyclicCombinationalGraph,
    /// A driver's optimized bytecode contains an instruction outside
    /// the JIT translator's narrow subset.  `driver` names the driven
    /// component, `instruction` the offending instruction's variant
    /// name.
    UntranslatableDriver {
        driver: SignalId,
        instruction: String,
    },
}

impl Disqualification {
    /// Short machine-readable reason code (the profiler verdict's
    /// `disqualified(reason)` payload).
    pub fn reason(&self) -> &'static str {
        match self {
            Disqualification::RegisterClockedByNonPrimary { .. } => "register_clocked_by_non_primary",
            Disqualification::CyclicCombinationalGraph => "cyclic_combinational_graph",
            Disqualification::UntranslatableDriver { .. } => "untranslatable_driver",
        }
    }
}

/// One driver entry in an eval region: the driven component, in the
/// plan's (topological) order, with its deduped input signals.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DriverPlan {
    /// The component the driver commits to.
    pub driver: SignalId,
    /// The signals the driver's bytecode reads (deduped, in the stage-A
    /// schedule's order).  For an alias sink this is the peer alone.
    pub inputs: Vec<SignalId>,
}

/// One region of the static cycle schedule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Region {
    /// A settlement of combinational drivers, in topological order.
    Eval {
        /// The region's drivers.
        drivers: Vec<DriverPlan>,
        /// True when the region runs after the transfer point (the
        /// combinational tail downstream of the transferred registers),
        /// false for the pre-transfer eval region.
        after_transfer: bool,
    },
    /// The transfer point: all registers whose clock rises, in the
    /// engine's existing transfer order.
    Transfer {
        /// The registers to transfer.
        registers: Vec<SignalId>,
        /// The clock signal whose rising edge causes the transfer.
        clock: SignalId,
    },
}

/// The static whole-cycle schedule for a qualified design.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CyclePlan {
    /// The regions, in execution order: the pre-transfer eval region,
    /// the transfer point, then the post-transfer eval region (for a
    /// single-clock design).
    pub regions: Vec<Region>,
}

/// The load-time qualification verdict.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CyclePlanResult {
    /// The design qualifies; the plan is the static whole-cycle
    /// schedule.
    Qualified(CyclePlan),
    /// The design does not qualify; the reason names the offending
    /// signal path.
    Disqualified(Disqualification),
}

impl CyclePlanResult {
    /// Integer status code for the profiler counter: 1 = qualified,
    /// 2 = disqualified (register clock), 3 = disqualified (cyclic
    /// combinational graph), 4 = disqualified (untranslatable driver).
    /// Zero would mean the verdict is pending (no clock attached).
    #[cfg_attr(not(feature = "sim-profile"), allow(dead_code))]
    pub fn status_code(&self) -> u64 {
        match self {
            CyclePlanResult::Qualified(_) => 1,
            CyclePlanResult::Disqualified(Disqualification::RegisterClockedByNonPrimary { .. }) => 2,
            CyclePlanResult::Disqualified(Disqualification::CyclicCombinationalGraph) => 3,
            CyclePlanResult::Disqualified(Disqualification::UntranslatableDriver { .. }) => 4,
        }
    }

    /// Verdict string for the profiler counter and tests:
    /// `qualified` or `disqualified(reason)`.
    pub fn verdict(&self) -> String {
        match self {
            CyclePlanResult::Qualified(_) => "qualified".to_string(),
            CyclePlanResult::Disqualified(d) => format!("disqualified({})", d.reason()),
        }
    }
}

/// One instruction, tested against the JIT translator's narrow subset
/// (`vm::jit::eligible` mirrored for the whole-design qualification).
/// A `Load` must additionally target a narrow signal (its live value
/// is always the inline `Small` variant).  On failure returns the
/// offending instruction's variant name.
fn translatable(ins: &Ins, signal_narrow: &[bool]) -> Result<(), String> {
    match ins {
        Ins::Load { signal, dst, .. } => {
            if !matches!(dst, Dst::Narrow(_)) {
                return Err("Load".to_string());
            }
            if !signal_narrow.get(signal.index()).copied().unwrap_or(false) {
                return Err("Load".to_string());
            }
            Ok(())
        }
        Ins::Zero { dst, .. }
        | Ins::Splice { dst, .. }
        | Ins::Zext { dst, .. }
        | Ins::ExtractWide { dst, .. } => {
            if !matches!(dst, Dst::Narrow(_)) {
                return Err(ins_variant(ins));
            }
            Ok(())
        }
        Ins::WideConst { .. }
        | Ins::TestTagWide { .. }
        | Ins::BinWide { .. }
        | Ins::UnWide { .. }
        | Ins::EncodeCtorWide { .. }
        | Ins::IndexWide { .. }
        | Ins::IndexDynWide { .. }
        | Ins::IndexRangeWide { .. }
        | Ins::ZextWide { .. }
        | Ins::SextWide { .. }
        | Ins::MoveWide { .. }
        | Ins::AnyWide { .. }
        | Ins::AllWide { .. }
        | Ins::RetWide { .. }
        | Ins::Trap => Err(ins_variant(ins)),
        Ins::Sext { out_width, .. } => {
            if *out_width > 64 {
                return Err("Sext".to_string());
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// The instruction's variant name, from its Debug output.
fn ins_variant(ins: &Ins) -> String {
    let s = format!("{ins:?}");
    let name = s
        .split(|c: char| c == ' ' || c == '(')
        .next()
        .unwrap_or("unknown");
    name.to_string()
}

/// Compute the whole-design qualification verdict and, for a qualified
/// design, the static cycle schedule.
///
/// * `circuit`: the design's elaborated dataflow tables.
/// * `program`: the compiled drivers (with the stage-A topo schedule).
/// * `primary_clock`: the signal `attach_clock` attaches as the
///   free-running clock.
pub(in crate::sim) fn plan(
    circuit: &Circuit,
    program: &Program,
    primary_clock: SignalId,
) -> CyclePlanResult {
    let n_components = circuit.elaboration.components().len();

    // Clause 2: acyclic combinational graph.  A cyclic design has no
    // topological schedule at all.
    let Some(topo) = program.topo_schedule() else {
        return CyclePlanResult::Disqualified(Disqualification::CyclicCombinationalGraph);
    };

    // Clause 1: single-clock discipline.  Every register must be
    // clocked by the primary clock.  Walk every clock's register list;
    // any register listed under a non-primary clock disqualifies.
    let primary_idx = primary_clock.index();
    for (clock_idx, regs) in circuit.clock_sensitivities.iter().enumerate() {
        if clock_idx == primary_idx || regs.is_empty() {
            continue;
        }
        let register = regs[0];
        return CyclePlanResult::Disqualified(
            Disqualification::RegisterClockedByNonPrimary {
                register,
                clock: SignalId::from_index(clock_idx),
                primary: primary_clock,
            },
        );
    }
    // Every register must be clocked by the primary clock: a register
    // that lists no clock at all has no schedule point.  (Registers
    // clocked by a non-primary signal were already rejected above, so
    // any register missing from the primary's list is unclocked.)
    let primary_regs = &circuit.clock_sensitivities[primary_idx];
    let n_registers = circuit.registers.iter().filter(|&&r| r).count();
    if primary_regs.len() != n_registers {
        // Find a concrete offending register for the diagnostic.
        let clocked: std::collections::HashSet<SignalId> =
            primary_regs.iter().copied().collect();
        for i in 0..n_components {
            if circuit.registers[i] {
                let register = SignalId::from_index(i);
                if !clocked.contains(&register) {
                    // The register lists no clock: its transfer timing
                    // is a runtime event, not a schedule point.
                    return CyclePlanResult::Disqualified(
                        Disqualification::RegisterClockedByNonPrimary {
                            register,
                            clock: primary_clock,
                            primary: primary_clock,
                        },
                    );
                }
            }
        }
    }

    // Clause 3: translatable drivers.  Every driver with bytecode must
    // contain only instructions in the JIT translator's narrow subset,
    // and every `Load` must target a narrow signal.
    let signal_narrow: Vec<bool> = (0..n_components)
        .map(|i| {
            let typ = circuit
                .elaboration
                .component(SignalId::from_index(i))
                .typ();
            circuit.type_layout_index.type_width(&typ) <= 64
        })
        .collect();
    for comp_idx in 0..n_components {
        // Constant-folded drivers commit through the constant-unit
        // skip; alias sinks commit the peer copy; undriven components
        // do nothing.  None of these carry bytecode to translate.
        if program.entry[comp_idx].is_none() || program.const_values[comp_idx].is_some() {
            continue;
        }
        let range = program.entry[comp_idx].unwrap();
        let body = &program.code[range.start as usize..(range.start + range.len) as usize];
        for ins in body {
            if let Err(variant) = translatable(ins, &signal_narrow) {
                return CyclePlanResult::Disqualified(Disqualification::UntranslatableDriver {
                    driver: SignalId::from_index(comp_idx),
                    instruction: variant.to_string(),
                });
            }
        }
    }

    // The plan.  Regions:
    //
    // * pre-transfer eval: every driver in topo order.  Register
    //   drivers commit only their pending next-value (never a live
    //   value mid-flow), so they are inert for propagation and sit
    //   wherever their inputs put them; they must run before the
    //   transfer so the transfer captures their commit.
    // * transfer: every register clocked by the primary clock, in the
    //   engine's existing transfer order (`circuit.clock_sensitivities`
    //   for the primary clock).
    // * post-transfer eval (the combinational tail): every driver that
    //   transitively depends on a register's live value.  A register's
    //   live value changes only at the transfer, so drivers in its
    //   forward cone recompute with the new value; drivers outside the
    //   cone (fed only by primary inputs or by drivers outside the
    //   cone) are already settled by the pre-transfer region.
    let pre_transfer = topo
        .order()
        .iter()
        .map(|&d| DriverPlan {
            driver: SignalId::from_index(d as usize),
            inputs: topo.inputs(d as usize)
                .iter()
                .map(|&s| SignalId::from_index(s as usize))
                .collect(),
        })
        .collect::<Vec<_>>();

    let tail = downstream_of_registers(circuit, topo, n_components);
    let post_transfer = topo
        .order()
        .iter()
        .filter(|&&d| tail[d as usize])
        .map(|&d| DriverPlan {
            driver: SignalId::from_index(d as usize),
            inputs: topo.inputs(d as usize)
                .iter()
                .map(|&s| SignalId::from_index(s as usize))
                .collect(),
        })
        .collect::<Vec<_>>();

    let plan = CyclePlan {
        regions: vec![
            Region::Eval {
                drivers: pre_transfer,
                after_transfer: false,
            },
            Region::Transfer {
                registers: circuit.clock_sensitivities[primary_idx].clone(),
                clock: primary_clock,
            },
            Region::Eval {
                drivers: post_transfer,
                after_transfer: true,
            },
        ],
    };
    CyclePlanResult::Qualified(plan)
}

/// Which components must be re-evaluated after the register transfer
/// point: every component transitively downstream of a register's live
/// value.
///
/// Seeds every register (its live value changes at the transfer), then
/// propagates over the "reader" edges: a driver that reads a dirty
/// signal is dirty, and if the driver propagates (its commit writes a
/// live value, i.e. it is not a register), its output signal becomes
/// dirty.  Register drivers and register alias copies write only their
/// pending next-value, so propagation stops at them.
fn downstream_of_registers(
    circuit: &Circuit,
    topo: &TopoSchedule,
    n_components: usize,
) -> Vec<bool> {
    // Reverse adjacency: for each signal, the drivers that read it
    // (ascending component order, from the schedule's inputs).
    let mut readers: Vec<Vec<usize>> = vec![Vec::new(); n_components];
    for &d in topo.order() {
        let d_idx = d as usize;
        for &s in topo.inputs(d_idx) {
            readers[s as usize].push(d_idx);
        }
    }

    let mut dirty = vec![false; n_components];
    let mut queue: VecDeque<usize> = VecDeque::new();
    for (i, &is_reg) in circuit.registers.iter().enumerate() {
        if is_reg {
            dirty[i] = true;
            queue.push_back(i);
        }
    }
    while let Some(s) = queue.pop_front() {
        for &d in &readers[s] {
            if dirty[d] {
                continue;
            }
            dirty[d] = true;
            // A register's driver commits only the pending next-value,
            // so its run does not change any live value: readers of the
            // register's live value are unaffected by it.
            if !circuit.registers[d] {
                queue.push_back(d);
            }
        }
    }
    dirty
}
