//! In-process phase attribution for the simulator engine, behind the
//! `sim-profile` cargo feature.
//!
//! Kernel profilers (`perf`, gdb) are blocked in the common sandbox,
//! so this module is the measurement instrument that ranks simulator
//! optimization work (see `PERF-IDEAS.md` item 1).  With the feature
//! off, every instrumentation call site is `#[cfg]`-gated away and
//! this module is not compiled at all: zero cost, byte-identical
//! behavior.
//!
//! Granularity rules:
//!
//! - `Instant` timing happens at phase granularity only: once per
//!   `Sim::run`, per scheduler event (`next_after_delay`), per `flow`
//!   settlement, per watcher-dispatch pass, and per rising-edge
//!   register-transfer loop (sampled).  A phase timer costs about two
//!   `Instant` reads (~54 ns per entry pair on a typical x86_64 Linux
//!   box); nothing here times *inside* the per-driver path, where a
//!   single ~26 ns read would distort a ~5 ns driver eval.
//! - Per-driver work (`eval_driver`, `State::set_val`,
//!   `State::transfer`) is counted exactly, and timed only by stride
//!   sampling: every 256th call (every 8th rising-edge transfer loop)
//!   is timed with one `Instant` pair and its raw interval is
//!   accumulated unscaled, together with the sample count.  The
//!   amortized per-call instrumentation cost is well under a
//!   nanosecond.
//!
//! Estimating totals from the samples (done by the caller, e.g.
//! `virdant/examples/simprof.rs`): a sampled interval embeds roughly
//! one clock read past the measured body, so the per-call estimate is
//! `max(0, sample_ns / samples - eps)` where `eps` is the interval of
//! two back-to-back `Instant::now()` reads, measured once by the
//! caller.  Multiply by the exact call count for the category total.
//! For the exact phase timers, `phase_ns` likewise carries about one
//! `eps` per entry; the entry counts are in the snapshot.
//!
//! The counters are plain `Relaxed` atomics in one static struct; the
//! engine is single-threaded (`Rc` callbacks, no `Send`), so there is
//! no contention.  Consume the counters with [`snapshot`] (returns a
//! plain struct for the caller to format) and [`reset`].

use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

/// Stride-sampling parameter: time every `1 << SAMPLE_SHIFT`th call.
const SAMPLE_SHIFT: u32 = 8;
const SAMPLE_MASK: u64 = (1u64 << SAMPLE_SHIFT) - 1;

/// Stride for the rising-edge transfer-loop sampler: the loop is once
/// per clock cycle, so a coarser stride keeps its overhead negligible.
const LOOP_SAMPLE_SHIFT: u32 = 3;
const LOOP_SAMPLE_MASK: u64 = (1u64 << LOOP_SAMPLE_SHIFT) - 1;

/// All profile counters.  `*_ns` fields are elapsed nanoseconds
/// (exact phase timers) or raw sampled intervals (the `*_sample_ns`
/// fields); the rest are exact event counts.  See the field docs for
/// what each covers.
struct Counters {
    /// Total elapsed in `Sim::run` (lifecycle drains + timed-event
    /// loop + callbacks + everything the phases below sit inside).
    run_ns: AtomicU64,
    /// Elapsed in `Scheduler::next_after_delay`: heap pop plus time
    /// advance, one entry per fired timed event.
    sched_ns: AtomicU64,
    /// Elapsed in `Sim::flow` settlements (propagation to fixpoint,
    /// including the driver evals and set_val commits inside).
    flow_ns: AtomicU64,
    /// Elapsed in `Sim::dispatch_pass`: watcher-table sweeps.
    dispatch_ns: AtomicU64,

    /// Raw intervals of the sampled `eval_driver` calls, plus how
    /// many samples.  Estimate via the formula in the module docs.
    eval_sample_ns: AtomicU64,
    eval_samples: AtomicU64,
    /// Raw intervals of the sampled `State::set_val` calls (all
    /// callers), plus how many samples.
    commit_sample_ns: AtomicU64,
    commit_samples: AtomicU64,
    /// Raw intervals of the sampled timed-heap pushes in
    /// `Scheduler::register`, plus how many samples.
    push_sample_ns: AtomicU64,
    push_samples: AtomicU64,
    /// Raw intervals of the sampled rising-edge transfer loops in
    /// `Sim::transfer_clock` (each covers that edge's register
    /// transfers), plus how many samples.
    xfer_sample_ns: AtomicU64,
    xfer_samples: AtomicU64,

    /// Timed events popped and fired (`next_after_delay` returned
    /// `Some`).  With free-running clocks on the fast path, these are
    /// the user (`at`/`after`) events only: clock edges are stepped by
    /// the run loop directly and counted in `clock_edges` instead.
    events_popped: AtomicU64,
    /// Clock edges stepped by the run loop's free-running fast path
    /// (`Sim::step_clock`): no heap push, no heap pop, no scheduler
    /// dispatch.  One per half period per attached clock while no user
    /// event beats the edge.
    clock_edges: AtomicU64,
    /// Timed-heap pushes in `Scheduler::register` (`AfterDelay`
    /// registrations).  One per user event; clock edges on the
    /// free-running fast path do not push.  Ticks the push sampler.
    sched_push_calls: AtomicU64,
    /// Number of `flow` settlements.
    flow_calls: AtomicU64,
    /// Dirty-list pops inside `flow` (changed signals propagated).
    dirty_pops: AtomicU64,
    /// Driver evals executed in `eval_driver`, all on the compiled VM
    /// (the interpreter fallback is gone).  Ticks the eval sampler, so
    /// it counts construction-time `flow_all` evals too.
    evals_total: AtomicU64,
    /// `eval_driver` calls answered from the constant-fold table
    /// without running any engine.
    const_skips: AtomicU64,
    /// `State::set_val` calls (all callers).  Ticks the commit
    /// sampler.
    set_val_calls: AtomicU64,
    /// `State::set_val` calls that actually wrote (value changed).
    set_val_writes: AtomicU64,
    /// `State::transfer` calls (register transfers), counted in bulk
    /// at the call sites: `n` per rising edge that transfers `n`
    /// registers, so one counter update per clock edge, not per
    /// register.
    transfers: AtomicU64,
    /// `State::transfer` calls that actually wrote (value changed);
    /// only paid on a real write, which is rare in steady state.
    transfer_writes: AtomicU64,
    /// Rising edges seen by `Sim::transfer_clock`: one per clock cycle
    /// for a single-clock design, so a cycle-count proxy.  Ticks the
    /// transfer-loop sampler.
    rising_edges: AtomicU64,
    /// Watcher-dispatch passes (`dispatch_pass` entries, fired or not).
    dispatch_passes: AtomicU64,
    /// Watcher callbacks that fired.
    watcher_fires: AtomicU64,
}

static COUNTERS: Counters = Counters {
    run_ns: AtomicU64::new(0),
    sched_ns: AtomicU64::new(0),
    flow_ns: AtomicU64::new(0),
    dispatch_ns: AtomicU64::new(0),
    eval_sample_ns: AtomicU64::new(0),
    eval_samples: AtomicU64::new(0),
    commit_sample_ns: AtomicU64::new(0),
    commit_samples: AtomicU64::new(0),
    push_sample_ns: AtomicU64::new(0),
    push_samples: AtomicU64::new(0),
    xfer_sample_ns: AtomicU64::new(0),
    xfer_samples: AtomicU64::new(0),
    events_popped: AtomicU64::new(0),
    clock_edges: AtomicU64::new(0),
    sched_push_calls: AtomicU64::new(0),
    flow_calls: AtomicU64::new(0),
    dirty_pops: AtomicU64::new(0),
    evals_total: AtomicU64::new(0),
    const_skips: AtomicU64::new(0),
    set_val_calls: AtomicU64::new(0),
    set_val_writes: AtomicU64::new(0),
    transfers: AtomicU64::new(0),
    transfer_writes: AtomicU64::new(0),
    rising_edges: AtomicU64::new(0),
    dispatch_passes: AtomicU64::new(0),
    watcher_fires: AtomicU64::new(0),
};

/// Scoped timer for one engine phase; accumulates the elapsed time
/// into its phase's counter on drop.  Created only by the `time_*`
/// constructors below; call sites just bind it and let it drop.
pub struct PhaseGuard {
    slot: &'static AtomicU64,
    start: Instant,
}

impl Drop for PhaseGuard {
    #[inline]
    fn drop(&mut self) {
        let ns = self.start.elapsed().as_nanos() as u64;
        self.slot.fetch_add(ns, Ordering::Relaxed);
    }
}

macro_rules! phase_timer {
    ($name:ident, $ns:ident) => {
        /// Time one entry into the phase; the guard accumulates on
        /// drop.  Drop the binding at the end of the phase's scope.
        #[inline]
        pub fn $name() -> PhaseGuard {
            PhaseGuard { slot: &COUNTERS.$ns, start: Instant::now() }
        }
    };
    ($name:ident, $ns:ident, $calls:ident) => {
        /// Time one entry into the phase, counting the entry; the
        /// guard accumulates on drop.
        #[inline]
        pub fn $name() -> PhaseGuard {
            COUNTERS.$calls.fetch_add(1, Ordering::Relaxed);
            PhaseGuard { slot: &COUNTERS.$ns, start: Instant::now() }
        }
    };
}

phase_timer!(time_run, run_ns);
phase_timer!(time_sched, sched_ns);
phase_timer!(time_flow, flow_ns, flow_calls);
phase_timer!(time_dispatch, dispatch_ns, dispatch_passes);

/// Scoped sampled timer for per-driver-path work; accumulates the raw
/// interval (unscaled) and one sample count on drop.  Created only by
/// the `sample_*` constructors below; call sites just bind it and let
/// it drop (the `None` case costs a branch).
pub struct SampleGuard {
    slot: &'static AtomicU64,
    samples: &'static AtomicU64,
    start: Instant,
}

impl Drop for SampleGuard {
    #[inline]
    fn drop(&mut self) {
        let ns = self.start.elapsed().as_nanos() as u64;
        self.slot.fetch_add(ns, Ordering::Relaxed);
        self.samples.fetch_add(1, Ordering::Relaxed);
    }
}

macro_rules! sampler {
    ($name:ident, $tick:ident, $ns:ident, $samples:ident, $stride:expr) => {
        /// Sampled timing for one per-driver-path category (see the
        /// module docs for the stride and the estimator formula):
        /// every stride-th call is timed with one `Instant` pair and
        /// its raw interval accumulated unscaled.  The tick fetch-add
        /// doubles as the category's exact call count, so an
        /// unsampled call costs one counter update.
        #[inline]
        pub fn $name() -> Option<SampleGuard> {
            let tick = COUNTERS.$tick.fetch_add(1, Ordering::Relaxed);
            if tick & $stride == 0 {
                Some(SampleGuard {
                    slot: &COUNTERS.$ns,
                    samples: &COUNTERS.$samples,
                    start: Instant::now(),
                })
            } else {
                None
            }
        }
    };
}

sampler!(sample_eval, evals_total, eval_sample_ns, eval_samples, SAMPLE_MASK);
sampler!(sample_set_val, set_val_calls, commit_sample_ns, commit_samples, SAMPLE_MASK);
sampler!(sample_sched_push, sched_push_calls, push_sample_ns, push_samples, SAMPLE_MASK);
sampler!(sample_xfer_loop, rising_edges, xfer_sample_ns, xfer_samples, LOOP_SAMPLE_MASK);

/// Count an `eval_driver` call answered from the constant-fold table.
#[inline]
pub fn bump_const_skip() {
    COUNTERS.const_skips.fetch_add(1, Ordering::Relaxed);
}

/// Count a fired timed event (`next_after_delay` returned `Some`).
#[inline]
pub fn bump_events_popped() {
    COUNTERS.events_popped.fetch_add(1, Ordering::Relaxed);
}

/// Count a clock edge stepped by the run loop's free-running fast
/// path (`Sim::step_clock`).
#[inline]
pub fn bump_clock_edge() {
    COUNTERS.clock_edges.fetch_add(1, Ordering::Relaxed);
}

/// Count a dirty-list pop inside `flow`.
#[inline]
pub fn bump_dirty_pop() {
    COUNTERS.dirty_pops.fetch_add(1, Ordering::Relaxed);
}

/// Count `n` register transfers in bulk (one counter update per
/// rising edge instead of one per register).
#[inline]
pub fn bump_transfers(n: u64) {
    COUNTERS.transfers.fetch_add(n, Ordering::Relaxed);
}

/// Count a `State::set_val` call that actually wrote.
#[inline]
pub fn bump_set_val_write() {
    COUNTERS.set_val_writes.fetch_add(1, Ordering::Relaxed);
}

/// Count a `State::transfer` call that actually wrote.
#[inline]
pub fn bump_transfer_write() {
    COUNTERS.transfer_writes.fetch_add(1, Ordering::Relaxed);
}

/// Count a fired watcher callback.
#[inline]
pub fn bump_watcher_fire() {
    COUNTERS.watcher_fires.fetch_add(1, Ordering::Relaxed);
}

/// A consistent copy of every counter, taken by [`snapshot`].  Plain
/// integers: the caller formats them (and derives the sampled
/// estimates; see the estimator formula in the module docs).
#[derive(Debug, Clone, Copy, Default)]
pub struct Snapshot {
    pub run_ns: u64,
    pub sched_ns: u64,
    pub flow_ns: u64,
    pub dispatch_ns: u64,
    pub eval_sample_ns: u64,
    pub eval_samples: u64,
    pub commit_sample_ns: u64,
    pub commit_samples: u64,
    pub push_sample_ns: u64,
    pub push_samples: u64,
    pub xfer_sample_ns: u64,
    pub xfer_samples: u64,
    pub events_popped: u64,
    pub clock_edges: u64,
    pub sched_push_calls: u64,
    pub flow_calls: u64,
    pub dirty_pops: u64,
    pub evals_total: u64,
    pub const_skips: u64,
    pub set_val_calls: u64,
    pub set_val_writes: u64,
    pub transfers: u64,
    pub transfer_writes: u64,
    pub rising_edges: u64,
    pub dispatch_passes: u64,
    pub watcher_fires: u64,
}

impl Snapshot {
    /// Every executed driver eval ran on the compiled VM (the
    /// interpreter fallback is gone), so `evals_total` is the VM
    /// count.
    pub fn evals_vm(&self) -> u64 {
        self.evals_total
    }
}

/// Copy every counter.  Call on the thread that ran the sim.
pub fn snapshot() -> Snapshot {
    Snapshot {
        run_ns: COUNTERS.run_ns.load(Ordering::Relaxed),
        sched_ns: COUNTERS.sched_ns.load(Ordering::Relaxed),
        flow_ns: COUNTERS.flow_ns.load(Ordering::Relaxed),
        dispatch_ns: COUNTERS.dispatch_ns.load(Ordering::Relaxed),
        eval_sample_ns: COUNTERS.eval_sample_ns.load(Ordering::Relaxed),
        eval_samples: COUNTERS.eval_samples.load(Ordering::Relaxed),
        commit_sample_ns: COUNTERS.commit_sample_ns.load(Ordering::Relaxed),
        commit_samples: COUNTERS.commit_samples.load(Ordering::Relaxed),
        push_sample_ns: COUNTERS.push_sample_ns.load(Ordering::Relaxed),
        push_samples: COUNTERS.push_samples.load(Ordering::Relaxed),
        xfer_sample_ns: COUNTERS.xfer_sample_ns.load(Ordering::Relaxed),
        xfer_samples: COUNTERS.xfer_samples.load(Ordering::Relaxed),
        events_popped: COUNTERS.events_popped.load(Ordering::Relaxed),
        clock_edges: COUNTERS.clock_edges.load(Ordering::Relaxed),
        sched_push_calls: COUNTERS.sched_push_calls.load(Ordering::Relaxed),
        flow_calls: COUNTERS.flow_calls.load(Ordering::Relaxed),
        dirty_pops: COUNTERS.dirty_pops.load(Ordering::Relaxed),
        evals_total: COUNTERS.evals_total.load(Ordering::Relaxed),
        const_skips: COUNTERS.const_skips.load(Ordering::Relaxed),
        set_val_calls: COUNTERS.set_val_calls.load(Ordering::Relaxed),
        set_val_writes: COUNTERS.set_val_writes.load(Ordering::Relaxed),
        transfers: COUNTERS.transfers.load(Ordering::Relaxed),
        transfer_writes: COUNTERS.transfer_writes.load(Ordering::Relaxed),
        rising_edges: COUNTERS.rising_edges.load(Ordering::Relaxed),
        dispatch_passes: COUNTERS.dispatch_passes.load(Ordering::Relaxed),
        watcher_fires: COUNTERS.watcher_fires.load(Ordering::Relaxed),
    }
}

/// Zero every counter (between runs, or to exclude construction-time
/// work from a measurement).
pub fn reset() {
    COUNTERS.run_ns.store(0, Ordering::Relaxed);
    COUNTERS.sched_ns.store(0, Ordering::Relaxed);
    COUNTERS.flow_ns.store(0, Ordering::Relaxed);
    COUNTERS.dispatch_ns.store(0, Ordering::Relaxed);
    COUNTERS.eval_sample_ns.store(0, Ordering::Relaxed);
    COUNTERS.eval_samples.store(0, Ordering::Relaxed);
    COUNTERS.commit_sample_ns.store(0, Ordering::Relaxed);
    COUNTERS.commit_samples.store(0, Ordering::Relaxed);
    COUNTERS.push_sample_ns.store(0, Ordering::Relaxed);
    COUNTERS.push_samples.store(0, Ordering::Relaxed);
    COUNTERS.xfer_sample_ns.store(0, Ordering::Relaxed);
    COUNTERS.xfer_samples.store(0, Ordering::Relaxed);
    COUNTERS.events_popped.store(0, Ordering::Relaxed);
    COUNTERS.clock_edges.store(0, Ordering::Relaxed);
    COUNTERS.sched_push_calls.store(0, Ordering::Relaxed);
    COUNTERS.flow_calls.store(0, Ordering::Relaxed);
    COUNTERS.dirty_pops.store(0, Ordering::Relaxed);
    COUNTERS.evals_total.store(0, Ordering::Relaxed);
    COUNTERS.const_skips.store(0, Ordering::Relaxed);
    COUNTERS.set_val_calls.store(0, Ordering::Relaxed);
    COUNTERS.set_val_writes.store(0, Ordering::Relaxed);
    COUNTERS.transfers.store(0, Ordering::Relaxed);
    COUNTERS.transfer_writes.store(0, Ordering::Relaxed);
    COUNTERS.rising_edges.store(0, Ordering::Relaxed);
    COUNTERS.dispatch_passes.store(0, Ordering::Relaxed);
    COUNTERS.watcher_fires.store(0, Ordering::Relaxed);
}
