//! Event scheduler owning simulated time, the callback queue for timed
//! (`AfterDelay`) and lifecycle (`StartOfSimulation`/
//! `EndOfSimulation`) events, the FIFO sequence counter, and the
//! shutdown flag — mirroring a subset of VPI callback reasons.
//! Value-change watching is not queue-based: `Sim` keeps a persistent
//! watcher table swept in place at each settlement (see `Sim::unlock`).
//!
//! The queue is split in two: timed events live in a min-`BinaryHeap`
//! ordered by `(at_ps, seq)` — popping the soonest deadline, FIFO
//! tiebreak, is O(log n) instead of a linear min-scan plus `Vec::remove`
//! shift — while lifecycle events keep their registration (FIFO) order
//! in their own deque.  `Sim::run` drains them separately (lifecycle
//! before/after the timed loop), so the split changes nothing observable.

use crate::sim::Sim;
use crate::analysis::elaboration::SignalId;

use std::cmp::Ordering;
use std::collections::{BinaryHeap, VecDeque};

/// Event scheduler and dispatch state.  Owns simulated time, the
/// one-shot callback queues for timed and lifecycle events, the FIFO
/// tie-breaker counter, and the early-termination flag toggled by
/// `Sim::finish`.  Every registration pushes one entry into `timed`
/// (`AfterDelay`) or `lifecycle` (start/end); every fire removes one.
/// Value-change watchers live in `Sim`'s persistent watcher table
/// instead.  See `Event` for the trigger conditions.
#[derive(Debug)]
pub(super) struct Scheduler {
    time_ps: u64,
    next_seq: u64,
    shutdown_requested: bool,
    /// Timed events, min-ordered by `(at_ps, seq)`: earliest deadline
    /// pops first, same-deadline entries fire in registration (FIFO)
    /// order.  Popping is O(log n); the old linear min-scan + remove
    /// was O(n).
    timed: BinaryHeap<TimedEntry>,
    /// Lifecycle events (`StartOfSimulation`/`EndOfSimulation`), in
    /// registration order.  `drain_lifecycle` drains only this deque —
    /// today it partitions the whole queue and re-inserts the rest.
    lifecycle: VecDeque<EventCallback>,
}

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
    ///
    /// Kept for API compatibility, but not queue-dispatched: a
    /// `ValueChange` registration is routed into `Sim`'s persistent
    /// watcher table (equivalent to `Sim::on_change`) and swept in
    /// place at each settlement.
    ValueChange { signal: SignalId },
}


/// Timed-queue slot: an `AfterDelay` callback with its deadline and
/// sequence number, wrapped in an inverted `Ord` so `BinaryHeap` pops
/// the earliest deadline (FIFO-tiebroken) first.
#[derive(Debug)]
struct TimedEntry {
    at_ps: u64,
    seq: u64,
    cb: EventCallback,
}

impl PartialEq for TimedEntry {
    fn eq(&self, other: &Self) -> bool {
        self.at_ps == other.at_ps && self.seq == other.seq
    }
}

impl Eq for TimedEntry {}

impl PartialOrd for TimedEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimedEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        // BinaryHeap pops the *greatest* entry, so the inverted order
        // turns it into a min-heap over (at_ps, seq).
        other.at_ps.cmp(&self.at_ps).then(other.seq.cmp(&self.seq))
    }
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

/// A user-supplied callback.  Receives `&mut Sim` so it can read signals,
/// drive new values, register more callbacks, or call `finish()`.
pub type Callback = Box<dyn FnMut(&mut Sim) + 'static>;

impl Scheduler {
    pub(super) fn new() -> Scheduler {
        Scheduler {
            time_ps: 0,
            next_seq: 0,
            shutdown_requested: false,
            timed: BinaryHeap::new(),
            lifecycle: VecDeque::new(),
        }
    }

    pub(super) fn time_ps(&self) -> u64 {
        self.time_ps
    }

    /// Push `event_cb` onto the queue.  The single primitive on which
    /// `at`, `after`, `at_start`, and `at_end` are built; value-change
    /// events do not belong in the queue (register them through
    /// `Sim::register`, which routes them into the watcher table).
    /// Timed entries land in the `(at_ps, seq)` min-heap; lifecycle
    /// entries land in the FIFO deque.  Same-trigger entries fire in
    /// registration (FIFO) order.
    pub fn register(&mut self, event_cb: EventCallback) {
        if matches!(event_cb.event, Event::ValueChange { .. }) {
            panic!(
                "Event::ValueChange must be registered via Sim::register / Sim::on_change; \
                 the scheduler queue does not dispatch value-change events"
            );
        }
        let seq = self.next_seq;
        self.next_seq += 1;
        match event_cb.event {
            Event::AfterDelay { at_ps } => {
                self.timed.push(TimedEntry { at_ps, seq, cb: event_cb });
            }
            Event::StartOfSimulation | Event::EndOfSimulation => {
                self.lifecycle.push_back(event_cb);
            }
            Event::ValueChange { .. } => unreachable!(
                "Event::ValueChange is rejected by the check above"
            ),
        }
    }

    pub(super) fn request_shutdown(&mut self) {
        self.shutdown_requested = true;
    }

    pub(super) fn is_shutdown_requested(&self) -> bool {
        self.shutdown_requested
    }

    /// Drain the lifecycle entries matching `pred`, returning the
    /// callbacks in registration order; non-matching lifecycle entries
    /// stay queued, and the timed heap is not touched.  Draining
    /// `StartOfSimulation` also clears a pending `finish()` so the
    /// timed loop below still runs.
    pub(super) fn drain_lifecycle<F: Fn(&Event) -> bool>(&mut self, pred: F) -> Vec<Callback> {
        if pred(&Event::StartOfSimulation) {
            self.shutdown_requested = false;
        }
        let lifecycle = std::mem::take(&mut self.lifecycle);
        let mut taken: Vec<Callback> = Vec::new();
        for mut entry in lifecycle {
            if pred(&entry.event) {
                if let Some(cb) = entry.callback.take() {
                    taken.push(cb);
                }
            } else {
                self.lifecycle.push_back(entry);
            }
        }
        taken
    }

    /// Pop the soonest timed entry (earliest `at_ps`, FIFO tiebreak),
    /// advance simulated time to its deadline, and return its callback.
    /// `None` when the timed queue is empty.
    pub(super) fn next_after_delay(&mut self) -> Option<Callback> {
        let mut entry = self.timed.pop()?;
        if let Event::AfterDelay { at_ps } = entry.cb.event {
            self.advance_to(at_ps);
        }
        entry.cb.callback.take()
    }

    fn advance_to(&mut self, time_ps: u64) {
        self.time_ps = time_ps;
    }
}

impl std::fmt::Debug for EventCallback {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EventCallback")
            .field("event", &self.event)
            .field("callback", &"<Callback>")
            .finish()
    }
}
