//! Event scheduler owning simulated time, the callback queue for timed
//! (`AfterDelay`) and lifecycle (`StartOfSimulation`/
//! `EndOfSimulation`) events, the FIFO sequence counter, and the
//! shutdown flag — mirroring a subset of VPI callback reasons.
//! Value-change watching is not queue-based: `Sim` keeps a persistent
//! watcher table swept in place at each settlement (see `Sim::unlock`).

use crate::sim::Sim;
use crate::analysis::elaboration::SignalId;

/// Event scheduler and dispatch state.  Owns simulated time, the
/// one-shot callback queue for timed and lifecycle events, the FIFO
/// tie-breaker counter, and the early-termination flag toggled by
/// `Sim::finish`.  Every registration pushes one entry into `queue`;
/// every fire removes one.  Value-change watchers live in `Sim`'s
/// persistent watcher table instead.  See `Event` for the trigger
/// conditions.
#[derive(Debug)]
pub(super) struct Scheduler {
    time_ps: u64,
    next_seq: u64,
    shutdown_requested: bool,
    queue: Vec<QueueEntry>,
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


/// Internal queue slot: an `EventCallback` plus a monotonic sequence
/// number used for FIFO tie-breaking among same-trigger entries.
#[derive(Debug)]
struct QueueEntry {
    seq: u64,
    cb: EventCallback,
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
            queue: Vec::new(),
        }
    }

    pub(super) fn time_ps(&self) -> u64 {
        self.time_ps
    }

    /// Push `event_cb` onto the queue.  The single primitive on which
    /// `at`, `after`, `at_start`, and `at_end` are built; value-change
    /// events do not belong in the queue (register them through
    /// `Sim::register`, which routes them into the watcher table).
    /// Same-trigger entries fire in registration (FIFO) order.
    pub fn register(&mut self, event_cb: EventCallback) {
        if matches!(event_cb.event, Event::ValueChange { .. }) {
            panic!(
                "Event::ValueChange must be registered via Sim::register / Sim::on_change; \
                 the scheduler queue does not dispatch value-change events"
            );
        }
        let seq = self.next_seq;
        self.next_seq += 1;
        self.queue.push(QueueEntry { seq, cb: event_cb });
    }

    pub(super) fn request_shutdown(&mut self) {
        self.shutdown_requested = true;
    }

    pub(super) fn is_shutdown_requested(&self) -> bool {
        self.shutdown_requested
    }

    pub(super) fn drain_lifecycle<F: Fn(&Event) -> bool>(&mut self, pred: F) -> Vec<Callback> {
        if pred(&Event::StartOfSimulation) {
            self.shutdown_requested = false;
        }
        let queue = std::mem::take(&mut self.queue);
        let mut keep: Vec<QueueEntry> = Vec::with_capacity(queue.len());
        let mut to_process: Vec<QueueEntry> = Vec::new();
        for entry in queue {
            if pred(&entry.cb.event) {
                to_process.push(entry);
            } else {
                keep.push(entry);
            }
        }
        self.queue = keep;
        to_process.sort_by_key(|q| q.seq);
        to_process.into_iter().filter_map(|mut e| e.cb.callback.take()).collect()
    }

    pub(super) fn next_after_delay(&mut self) -> Option<Callback> {
        let idx = self.queue
            .iter()
            .enumerate()
            .filter_map(|(i, q)| match q.cb.event {
                Event::AfterDelay { at_ps } => Some((i, at_ps, q.seq)),
                _ => None,
            })
            .min_by(|a, b| a.1.cmp(&b.1).then(a.2.cmp(&b.2)))
            .map(|(i, _, _)| i);

        if let Some(idx) = idx {
            let mut entry = self.queue.remove(idx);
            if let Event::AfterDelay { at_ps } = entry.cb.event {
                self.advance_to(at_ps);
            }
            entry.cb.callback.take()
        } else {
            None
        }
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
