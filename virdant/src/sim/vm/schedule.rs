//! The static combinational schedule (stage A of the scheduled kernel).
//!
//! Built once at VM compile time from the compiled drivers:
//!
//! * each driver's input-signal list: the signals its optimized
//!   bytecode actually `Load`s (the peer alone for an alias copy), a
//!   deduped list of only real data reads;
//! * a topological order over the drivers' combinational dependency
//!   graph.  At stage A this order validates the graph and anchors the
//!   scheduling data model; a later cached straight-line schedule will
//!   consume it directly.
//!
//! At flow time the dynamic worklist loop reads the per-driver input
//! list and the per-signal change-epochs: a driver already evaluated
//! within a settlement whose triggering signal has not been re-written
//! since that evaluation would recommit the output it already
//! committed, so the visit is skipped.  The per-driver input list and
//! the epochs are exactly the data model a cached straight-line
//! schedule needs later.
//!
//! A design whose combinational graph has a cycle gets no schedule:
//! [`TopoSchedule::build`] returns `None`, `Program::schedule` stays
//! `None`, and `flow` keeps the dynamic worklist path, which reaches a
//! fixpoint on any graph.  The dynamic path is the safety net; a
//! schedule is an optimization, never a requirement.

/// The static schedule over one design's drivers.
///
/// Drivers and signals are both identified by `SignalId::index()`
/// (every driver is the component it drives).  `order` and `inputs`
/// cover exactly the components that have a driver expression or an
/// alias peer; everything else is inert for scheduling.
pub(in crate::sim) struct TopoSchedule {
    /// Every driver, in topological order: a driver's position is
    /// strictly after every driver whose output it (transitively)
    /// reads through a propagating edge.  Register drivers (whose
    /// commit writes only the pending next-value, never a live value
    /// mid-flow) and register alias copies have no propagating output,
    /// so they sit wherever their inputs put them and expand nothing.
    order: Vec<u32>,
    /// `pos[component]` is the component's position in `order`, or
    /// `NOT_SCHEDULED` for components that are not drivers.
    pos: Vec<u32>,
    /// `inputs[component]` is the driver's deduped input-signal list
    /// (empty for non-drivers): the signals its optimized bytecode
    /// actually loads, or the alias peer alone.
    inputs: Vec<Box<[u32]>>,
}

/// Sentinel `pos` value for components with no driver.
const NOT_SCHEDULED: u32 = u32::MAX;

impl TopoSchedule {
    /// The driver order (topological; see the field doc).
    pub(in crate::sim) fn order(&self) -> &[u32] {
        &self.order
    }

    /// The component's position in `order`, or `NOT_SCHEDULED` when it
    /// is not a driver.  Read by the debug validators (and the tests);
    /// the flow-time epoch skip works off `inputs` alone.
    pub(in crate::sim) fn pos(&self, component: usize) -> u32 {
        self.pos[component]
    }

    /// Debug validation of the schedule's internal consistency: `order`
    /// is a permutation (every entry in component range, `pos` its
    /// exact inverse), every component with a driver expression is
    /// scheduled, and every input list stays in component range.  The
    /// topological property itself is checked against the circuit at
    /// build time (see `compile_program`).
    pub(in crate::sim) fn validate(&self, n_components: usize, has_expr: &[bool]) {
        assert_eq!(self.pos.len(), n_components, "schedule: pos table size");
        assert_eq!(self.inputs.len(), n_components, "schedule: inputs table size");
        assert!(self.order.len() <= n_components, "schedule: order longer than the component table");
        for (position, &d) in self.order.iter().enumerate() {
            assert!((d as usize) < n_components, "schedule: order entry out of range");
            assert_eq!(self.pos[d as usize], position as u32, "schedule: pos is not order's inverse");
            // Alias sinks are scheduled without a driver expression
            // (their commit is the peer copy), so only the converse
            // direction is asserted below.
        }
        for c in 0..n_components {
            if has_expr[c] {
                assert_ne!(self.pos[c], NOT_SCHEDULED, "schedule: driver {c} not scheduled");
            }
            for &s in &*self.inputs[c] {
                assert!((s as usize) < n_components, "schedule: input out of range");
            }
        }
    }

    /// The driver's deduped input-signal list (empty for non-drivers).
    pub(in crate::sim) fn inputs(&self, component: usize) -> &[u32] {
        &self.inputs[component]
    }

    /// Build the schedule from per-signal driver facts, or `None` when
    /// the combinational graph has a cycle.
    ///
    /// All three tables are densely indexed by `SignalId::index()`:
    ///
    /// * `is_driver[s]`: `s` has a driver expression or an alias peer,
    ///   so `s` is a schedule node (its producer evaluates to it);
    /// * `propagates[s]`: `s` is a driver whose commit writes a live
    ///   value mid-flow (a combinational driver or combinational alias
    ///   copy).  Only these start propagation, so only these carry
    ///   graph edges out;
    /// * `reads[s]`: for a driver, the deduped input-signal list.
    ///
    /// The edge set is then: `s -> d` for every driver `d` with `s` in
    /// `reads[d]` and `propagates[s]`.  A register's live value only
    /// changes through a clock-edge transfer (which re-enters `flow` as
    /// a fresh dirty seed), never through its own driver's evaluation,
    /// so a register is a graph source, never a cycle member.
    pub(in crate::sim) fn build(
        n_signals: usize,
        is_driver: &[bool],
        propagates: &[bool],
        reads: &[Box<[u32]>],
    ) -> Option<TopoSchedule> {
        // Reverse adjacency: for a propagating driver `s`, the drivers
        // that read `s` (in ascending component order, since `reads`
        // is walked in ascending order).  Only propagating signals
        // carry edges out, so only they need a list.
        let mut readers: Vec<Vec<u32>> = vec![Vec::new(); n_signals];
        let mut indegree = vec![0u32; n_signals];
        let mut driver_count = 0usize;
        for d in 0..n_signals {
            if !is_driver[d] {
                continue;
            }
            driver_count += 1;
            for &s in &*reads[d] {
                debug_assert!((s as usize) < n_signals, "schedule: input out of range");
                if !propagates[s as usize] {
                    continue;
                }
                indegree[d] += 1;
                readers[s as usize].push(d as u32);
            }
        }

        // Kahn's algorithm, FIFO seeded in ascending component order
        // (and `readers` ascending), so the produced order is a
        // deterministic function of the graph.
        let mut queue: std::collections::VecDeque<u32> = std::collections::VecDeque::new();
        for d in 0..n_signals {
            if is_driver[d] && indegree[d] == 0 {
                queue.push_back(d as u32);
            }
        }
        let mut order = Vec::with_capacity(driver_count);
        while let Some(d) = queue.pop_front() {
            order.push(d);
            for &r in &readers[d as usize] {
                indegree[r as usize] -= 1;
                if indegree[r as usize] == 0 {
                    queue.push_back(r);
                }
            }
        }
        // Every driver must have been placed; leftovers sit on a cycle.
        if order.len() != driver_count {
            return None;
        }

        let mut pos = vec![NOT_SCHEDULED; n_signals];
        for (position, &d) in order.iter().enumerate() {
            pos[d as usize] = position as u32;
        }
        let inputs = reads.to_vec();
        Some(TopoSchedule { order, pos, inputs })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::analysis::elaboration::SignalId;
    fn id(index: usize) -> SignalId {
        SignalId::from_index(index)
    }

    /// Schedule builder over plain indexes: `is_driver` true for every
    /// index in `drivers`, everything propagates unless listed.
    fn build_with(
        n: usize,
        drivers: &[usize],
        reads: &[Box<[u32]>],
        inert: &[usize],
    ) -> Option<TopoSchedule> {
        let mut is_driver = vec![false; n];
        for &d in drivers {
            is_driver[d] = true;
        }
        let mut propagates = is_driver.clone();
        for &i in inert {
            propagates[i] = false;
        }
        TopoSchedule::build(n, &is_driver, &propagates, reads)
    }

    /// The diamond: A feeds B and C, both feed D.  The schedule must
    /// place B and C (in either relative order) strictly before D.
    #[test]
    fn diamond_orders_both_branches_before_the_join() {
        // 0 = A, 1 = B, 2 = C, 3 = D; all four are drivers.
        let reads = vec![
            Box::from([] as [u32; 0]),
            Box::from([0u32]),
            Box::from([0u32]),
            Box::from([1u32, 2]),
        ];
        let schedule = build_with(4, &[0, 1, 2, 3], &reads, &[]).expect("acyclic");
        assert_eq!(schedule.order().len(), 4);
        assert!(schedule.pos(1) < schedule.pos(3), "B before D");
        assert!(schedule.pos(2) < schedule.pos(3), "C before D");
        assert_eq!(schedule.pos(0), 0, "the source A comes first");
        assert_eq!(schedule.inputs(id(3).index()), &[1, 2], "D reads B and C");
    }

    /// A two-node combinational cycle gets no schedule (the dynamic
    /// worklist path stays the fallback), and a driver reading its own
    /// output is caught as the self-edge it is.
    #[test]
    fn cycles_are_rejected() {
        let reads = vec![
            Box::from([] as [u32; 0]),
            Box::from([2u32]),
            Box::from([1u32]),
        ];
        assert!(build_with(3, &[1, 2], &reads, &[]).is_none(), "2 <-> 3 cycle");

        let self_loop = vec![Box::from([] as [u32; 0]), Box::from([1u32])];
        assert!(build_with(2, &[1], &self_loop, &[]).is_none(), "self-edge");
    }

    /// Register drivers are leaves: they read live values (including
    /// their own register) but their commit starts no propagation, so
    /// they carry no out-edges and never make the graph cyclic.
    #[test]
    fn register_drivers_are_inert_leaves() {
        // 0 = a register (inert producer), 1 = its driver (reads 0),
        // 2 = a combinational driver reading the register's live value.
        let reads = vec![
            Box::from([] as [u32; 0]),
            Box::from([0u32]),
            Box::from([0u32]),
        ];
        let schedule = build_with(3, &[0, 1, 2], &reads, &[0]).expect("acyclic");
        assert_eq!(schedule.order().len(), 3);
        // The register's driver has a position (it is scheduled and
        // epoch-skippable), but nothing depends on its placement.
        assert_ne!(schedule.pos(1), NOT_SCHEDULED);
    }

    /// Non-drivers never appear in the schedule at all.
    #[test]
    fn non_drivers_are_not_scheduled() {
        let reads = vec![Box::from([] as [u32; 0]); 2];
        let schedule = build_with(2, &[], &reads, &[]).expect("acyclic");
        assert_eq!(schedule.order().len(), 0);
        assert_eq!(schedule.pos(0), NOT_SCHEDULED);
        assert_eq!(schedule.pos(1), NOT_SCHEDULED);
        assert!(schedule.inputs(0).is_empty());
    }
}
