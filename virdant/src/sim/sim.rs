//! The `Sim` facade: the single public simulator type.  It owns every
//! engine-agnostic piece of the runtime — the event scheduler, the
//! free-running clock table, the persistent watcher table with its
//! bucketed dispatch, simulated time, and the VCD offset/time-marker
//! hooks — and forwards value-level work to the engine selected at
//! construction (`Inner`).  The default is the VM engine
//! (`sim::vm::engine::VmSim`, compiled bytecode); `Sim::with_engine(..,
//! Engine::Legacy)` runs the tree-walking interpreter
//! (`sim::legacy::engine::LegacySim`).
//!
//! Engines expose value-level primitives only (`flow`, `flow_all`,
//! `set_without_flow`, `tick_prop`, `get`, `peek`, `circuit`, the
//! changed-set/watched bookkeeping, and the
//! `on_structural_change`/`on_clock_attached` hooks), so watcher
//! callbacks — `Box<dyn FnMut(&mut Sim)>` — are always invoked with
//! the facade, never with an engine.

use crate::analysis::symbols::SymbolId;
use crate::analysis::elaboration::{SignalId, Elaboration};
use crate::common::{ComponentKind, Width};
use crate::db::Db;
use crate::sim::bits::BitPat;
use crate::sim::layout::{TypeId, TypeLayoutIndex};
use crate::sim::scheduler::{Callback, Event, EventCallback, Scheduler};
use crate::sim::value::Value;
use crate::types::Type;
use std::cell::RefCell;
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use bstr::{BStr, BString};

/// Which simulation engine a [`Sim`] runs on.
///
/// `Vm` is the compiled-bytecode engine (`sim::vm::engine::VmSim`,
/// the default); `Legacy` is the tree-walking interpreter
/// (`sim::legacy::engine::LegacySim`, e.g. selected by
/// `vir run --legacy`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Engine {
    Vm,
    Legacy,
}

/// The engine behind a [`Sim`].
enum Inner {
    Vm(crate::sim::vm::engine::VmSim),
    Legacy(crate::sim::legacy::engine::LegacySim),
}

macro_rules! dispatch_mut {
    ($inner:expr, $engine:ident => $body:expr) => {
        match &mut $inner {
            Inner::Vm($engine) => $body,
            Inner::Legacy($engine) => $body,
        }
    };
}

macro_rules! dispatch_ref {
    ($inner:expr, $engine:ident => $body:expr) => {
        match &$inner {
            Inner::Vm($engine) => $body,
            Inner::Legacy($engine) => $body,
        }
    };
}

/// Strategy for choosing the value of uninitialized registers and
/// top-level incoming ports.  There are no X or Z values: every cell
/// always carries a fully-defined bit pattern.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum InitStrategy {
    /// Pseudorandom bit patterns (seeded from the wall clock).
    #[default]
    Random,
    /// Pseudorandom bit patterns, seeded from an explicit value: two
    /// sims built with the same seed over the same design carry
    /// bit-identical uninitialized values, so their traces are
    /// directly comparable (differential testing).
    Seeded(u64),
}

impl InitStrategy {
    /// The seed for the uninitialized-value RNG: a fresh wall-clock
    /// value for `Random`, the given value for `Seeded`.
    pub(super) fn rng_seed(&self) -> u64 {
        match self {
            InitStrategy::Random => initial_seed(),
            InitStrategy::Seeded(seed) => *seed,
        }
    }

    pub(super) fn value(&self, typ_id: TypeId, width: Width, rng: &mut u64) -> Value {
        match self {
            InitStrategy::Random | InitStrategy::Seeded(_) =>
                Value::new(typ_id, BitPat::random(width, rng)),
        }
    }
}

/// A seed for the random init strategy, derived from the wall clock so
/// two runs see different uninitialized values.
fn initial_seed() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0x9E3779B97F4A7C15)
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
    inner: Inner,
    sched: Scheduler,
    /// Free-running clocks registered by `attach_clock`.  Each edge is
    /// stepped directly by the run loop when it beats every queued
    /// user event; see `ClockHandle` and `Sim::run`.
    clocks: Vec<ClockHandle>,
    /// Persistent watcher table (`on_change`/`on_clock`): registered
    /// once, swept in place at each settlement.  No per-fire
    /// re-registration, no queue rebuild.
    watchers: Vec<Watcher>,
    /// Watchers bucketed by the signal they observe:
    /// Watcher buckets, densely indexed by the observed signal's
    /// index: `watcher_buckets[signal_index]` lists the registration
    /// indices (into `watchers`) of every watcher observing that
    /// signal, in registration order.  Built by `on_change`/`on_clock`
    /// (registration appends to the table and to the signal's bucket)
    /// and rebuilt after the shutdown retain prunes dead entries, so
    /// the buckets always cover exactly the live table.  A dispatch
    /// pass visits only the buckets of signals that changed since the
    /// last sweep (or the table itself, when the changed set
    /// outnumbers the table), so the sweep cost follows what could
    /// actually fire, not the table size.
    watcher_buckets: Vec<Vec<usize>>,
    /// Reusable queue of registration indices for one bucket-walk
    /// dispatch pass: the union of the changed signals' buckets,
    /// sorted ascending.  Cleared and refilled each pass, never
    /// reallocated.
    dispatch_queue: Vec<usize>,
    /// True while the watcher dispatch loop is running.  A nested
    /// `unlock()` (a callback's own `lock()`/`set()`/`unlock()`) must
    /// only settle the circuit (`flow`), never re-sweep the table —
    /// dispatch happens exactly once per settlement, in the outermost
    /// unlock.
    dispatching: bool,
    /// Optional callback fired whenever simulated time moves forward
    /// (`advance_time`), handed the new time in picoseconds.  The VCD
    /// dumper registers one so that pure time advancement — no value
    /// changes — still stamps the trace's timeline; without it, a
    /// trace's last segment would be truncated to the last change.
    /// Set by the VCD dumper; `None` otherwise.
    time_marker: Option<Box<dyn Fn(u64)>>,
    /// Offset (ps) added to the VCD timestamps of every non-`Clock`
    /// signal (see `vcd.rs`).  0 by default.  The poke tester sets this
    /// to a twentieth of its clock period so that tick-driven changes
    /// render a visible sliver after the clock edge that caused them,
    /// teaching that logic does not settle instantaneously.
    vcd_offset_ps: u64,
}

/// One free-running clock registered by `Sim::attach_clock`.
///
/// Replaces the self-re-arming `AfterDelay` toggle event the engine
/// used to schedule per half period: the run loop steps an edge
/// directly whenever it beats every queued user event, so a clock
/// cycle costs no heap push (with its boxed-callback allocation), no
/// heap pop, and no scheduler dispatch.
///
/// `next` is the pending edge's `(time_ps, seq)` pair, compared in the
/// scheduler heap's own order — earliest time, then FIFO sequence —
/// so a tie against a user event fires in exactly the order the edge
/// would have had as a heap entry.  It is `None` only while the edge
/// is being processed, mirroring the old heap state during the toggle
/// callback: the popped entry was gone and the next one was registered
/// only after the settlement, so a re-entrant `run()` from a
/// settlement callback saw no pending edge for this clock.
struct ClockHandle {
    signal: SignalId,
    half_period_ps: u64,
    next: Option<(u64, u64)>,
}

/// How a watcher decides to fire.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WatchKind {
    /// Fire when the observed value differs from the last one this
    /// watcher saw (`on_change`).
    Change,
    /// Fire on a 0→1 transition of bit 0 (`on_clock`).
    ClockRising,
}

/// One persistent entry in `Sim`'s watcher table.
struct Watcher {
    signal: SignalId,
    kind: WatchKind,
    /// The last value *this watcher* observed.  Per-watcher (not shared
    /// circuit state), so each watcher observes each value change
    /// exactly once, regardless of settlement/commit ordering.
    last: Value,
    /// False once the callback ran after `finish()` was requested: the
    /// watcher must not survive into a subsequent `run()`.
    alive: bool,
    /// `Rc<RefCell<_>>` so the callback can be invoked with `&mut Sim`
    /// while the table itself is only borrowed (the engine never
    /// re-registers a watcher; the shared cell exists purely for the
    /// borrow checker).
    callback: Rc<RefCell<Callback>>,
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

/// The decoded constructor of a union/enum [`Value`]: which
/// constructor was selected, and the typed payloads it carries.
///
/// Returned by [`Sim::decode_ctor`].  This is the public face of the
/// type-layout machinery: callers never see `TypeLayoutIndex`,
/// `TypeId`, or raw bit patterns.
#[derive(Clone, Debug)]
pub struct Ctor {
    /// The selected constructor's name, e.g. `"Valid"`.
    pub name: String,
    /// The constructor's data, one [`Value`] per type parameter, in
    /// declaration order.  Each payload carries its parameter's type,
    /// so it can be compared with `==` or, if it is itself a
    /// union/enum, decoded further with [`Sim::decode_ctor`].  Empty
    /// for constructors with no data (e.g. `Invalid`).
    pub payloads: Vec<Value>,
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
    /// Build a `Sim` with the default engine (`Engine::Vm`).
    pub fn new(db: &Db, top: SymbolId, strategy: InitStrategy) -> Sim {
        Self::with_engine(db, top, strategy, Engine::Vm)
    }

    /// Build a `Sim` on an explicit engine.
    pub fn with_engine(db: &Db, top: SymbolId, strategy: InitStrategy, engine: Engine) -> Sim {
        let inner = match engine {
            Engine::Vm => Inner::Vm(crate::sim::vm::engine::VmSim::new(db, top, &strategy)),
            Engine::Legacy => Inner::Legacy(crate::sim::legacy::engine::LegacySim::new(db, top, &strategy)),
        };

        Sim {
            inner,
            sched: Scheduler::new(),
            clocks: Vec::new(),
            watchers: Vec::new(),
            watcher_buckets: Vec::new(),
            dispatch_queue: Vec::new(),
            dispatching: false,
            time_marker: None,
            vcd_offset_ps: 0,
        }
    }

    pub(super) fn type_layout_index(&self) -> &TypeLayoutIndex {
        dispatch_ref!(self.inner, engine => &engine.circuit().type_layout_index)
    }

    /// The circuit's [`TypeLayoutIndex`]: the public seam for tools that
    /// need type information at runtime (bit widths, `Type` <-> `TypeId`
    /// resolution, constructor encoding/decoding) — e.g. the `vir poke`
    /// tester, which evaluates script expressions over [`Value`]s and
    /// renders them round-trippably.
    pub fn layouts(&self) -> &TypeLayoutIndex {
        self.type_layout_index()
    }

    /// Encode a constructor application as a [`Value`] of type `typ`:
    /// `ctor_name` selects a constructor of the union/enum `typ` by
    /// name; each argument is checked against its parameter's type
    /// (re-tagged) and encoded tag-first through the type layout.
    /// Returns `None` when `typ` is not a union/enum, the constructor
    /// name is unknown, or an argument has the wrong width.
    ///
    /// The symmetric counterpart of [`Sim::decode_ctor`] — the public
    /// face of [`TypeLayoutIndex::encode_ctor`].
    pub fn encode_ctor_by_name(
        &self,
        typ: &Type,
        ctor_name: &str,
        args: &[Value],
    ) -> Option<Value> {
        let typ_id = self.layouts().type_id(typ);
        let (ctor_sym, param_types) = self.layouts().ctor_slot_by_name(typ_id, ctor_name)?;
        if param_types.len() != args.len() {
            return None;
        }
        let mut arg_pats = Vec::with_capacity(args.len());
        for (arg, param_typ) in args.iter().zip(param_types.iter()) {
            let param_width = self.layouts().type_width(param_typ);
            if arg.width() != param_width {
                return None;
            }
            arg_pats.push(arg.with_typ(self.layouts().type_id(param_typ)).bitpat());
        }
        let bits = self.layouts().encode_ctor(typ_id, ctor_sym, &arg_pats)?;
        Some(Value::new(typ_id, bits))
    }

    /// Decode a [`Value`] of struct type into its fields, in
    /// declaration order: `(field name, field value)` pairs.  Returns
    /// `None` when the value's type is not a struct (or the bit pattern
    /// has the wrong width).
    ///
    /// The counterpart of [`Sim::decode_ctor`] for structs, which have
    /// no constructor to decode — the public face of
    /// [`TypeLayoutIndex::decode_struct`].
    pub fn decode_struct_fields(&self, value: &Value) -> Option<Vec<(String, Value)>> {
        let typ = self.layouts().type_of(value.typ_id()).clone();
        let fields = self.layouts().fields(&typ)?;
        let bits = self.layouts().decode_struct(&typ, &value.bitpat())?;
        Some(fields
            .iter()
            .zip(bits)
            .map(|(field, bits)| {
                let field_value = Value::new(self.layouts().type_id(&field.param_types[0]), bits);
                (String::from_utf8(field.name.to_vec()).unwrap_or_default(), field_value)
            })
            .collect())
    }

    /// Decode a value whose type is a union or enum into the
    /// constructor it currently is.  Returns `None` when the value's
    /// type is not a union/enum (e.g. a plain `Word` or `Bit` — there
    /// is no "variant" to decode), or the bit pattern is not a valid
    /// inhabitant of the type (unknown tag, non-zero padding above the
    /// payload; mid-simulation this can happen transiently, before the
    /// logic has settled).
    pub fn decode_ctor(&self, value: &Value) -> Option<Ctor> {
        let (slot, payload_bits) = self
            .type_layout_index()
            .decode_ctor(value.typ_id(), &value.bitpat())?;
        let payloads = slot
            .param_types
            .iter()
            .zip(payload_bits)
            .map(|(typ, bits)| Value::new(self.type_layout_index().type_id(typ), bits))
            .collect();
        let name = String::from_utf8(slot.name.to_vec()).unwrap_or_else(|_| {
            panic!("constructor name was not valid UTF-8: {:?}", slot.name)
        });
        Some(Ctor { name, payloads })
    }

    /// Set the offset (ps) added to every non-`Clock` signal's VCD
    /// timestamps (0 by default).  Clocks stamp at their true time: they
    /// are the reference the offset is measured from.  See `vcd.rs`.
    pub fn set_vcd_offset_ps(&mut self, ps: u64) {
        self.vcd_offset_ps = ps;
    }

    pub(crate) fn vcd_offset_ps(&self) -> u64 {
        self.vcd_offset_ps
    }

    /// Stream a VCD trace of every dumpable signal to `writer`.  The header
    /// and an initial `$dumpvars` snapshot are written synchronously; per-
    /// signal `on_change` watchers are registered to write subsequent value
    /// changes.  A final `flush()` is registered to fire from `Sim::run`'s
    /// end-of-simulation hook.  See `vcd.rs` for the bit-layout seam.
    pub fn attach_vcd<W: Write + 'static>(&mut self, writer: W) {
        crate::sim::vcd::attach(self, writer);
    }

    /// Apply one rising clock edge on `signal_id`: transfer every
    /// register this signal clocks (through the sensitivity graph),
    /// then settle (`flow`) and dispatch value-change watchers — the
    /// same settlement an edge applied through `set` would get.
    pub fn tick(&mut self, signal_id: SignalId) {
        dispatch_mut!(self.inner, engine => {
            engine.tick_prop(signal_id);
            engine.flow();
        });
        self.unlock();
    }

    /// Settle the circuit after a batch of writes: propagate (`flow`),
    /// then dispatch value-change watchers.
    ///
    /// Dispatch is single-phase and non-reentrant: a nested `unlock()`
    /// (from a watcher callback's own `lock()`/`set()`/`unlock()`) only
    /// runs `flow()`; the sweep happens exactly once, in the outermost
    /// unlock.  This makes double-firing structurally impossible — the
    /// old implementation re-ran the watcher scan inside nested unlocks
    /// while transitions were still uncommitted, re-draining freshly
    /// re-registered watchers (one extra callback fire per event).
    fn unlock(&mut self) {
        dispatch_mut!(self.inner, engine => engine.flow());

        if self.dispatching {
            return;
        }

        // Skip the sweep entirely when no watched signal's stored value
        // has changed since the last sweep pass that observed
        // everything: a watcher fires only on a value change (`Change`)
        // or a 0→1 transition of bit 0 (`ClockRising`), and both
        // require the observed signal's stored value to have changed,
        // so a settlement with no write to a watched signal cannot fire
        // anything.  The flag is raised by the engine `State`'s write
        // primitives on every write to a watched signal (watched-only
        // marking: unwatched signals have no watcher to observe them,
        // so their writes skip the changed-set bookkeeping entirely),
        // including `set_without_flow` (edge-driven register transfers
        // included) and the register transfers inside this sweep's own
        // callbacks; each sweep pass consumes it together with the
        // per-signal changed set.
        if self.changed_since_dispatch() {
            self.dispatching = true;
            // Sweep-and-fire loop: each pass fires, in registration
            // order, every watcher whose observed value differs from
            // the last value it saw — visiting only the watchers whose
            // signals were actually written since the previous pass
            // started.  Callbacks may drive signals (settled by their
            // nested flow-only unlocks) or register more watchers; the
            // loop ends when the changed set is empty, i.e. when a pass
            // has observed everything with no fresh writes — the same
            // quiescence the old fired-until-nothing loop reached,
            // without re-walking watchers over changes they already
            // observed.  The loop runs to quiescence even past
            // `finish()`: watchers firing after the shutdown request
            // die instead of persisting (as the old re-arm check did),
            // but their final cascades still fire.
            while self.changed_signal_count() != 0 {
                self.dispatch_pass();
            }
            self.dispatching = false;
        }

        // Watchers whose callback ran after `finish()` was requested do
        // not survive into a subsequent `run()` (the old one-shot queue
        // achieved this by skipping the re-register step).  Skipped
        // sweeps cannot affect it: a watcher only becomes dead inside
        // `dispatch_pass`, and a skipped sweep runs no callback, so the
        // table is unchanged either way.
        if self.sched.is_shutdown_requested() {
            self.watchers.retain(|w| w.alive);
            self.rebuild_buckets();
        }
    }

    /// One sweep-and-fire pass over the watcher table.  Only the
    /// watchers whose observed signal has an actual write pending
    /// since the previous pass started are visited, in registration
    /// order.  Each visited watcher compares the live value against
    /// its own last-seen value; watchers that observe a change fire
    /// (for `ClockRising`, only on a 0→1 transition of bit 0), each
    /// updating its own last-seen value as it fires.
    ///
    /// Order-equivalence with the old whole-table sweep: a watcher can
    /// only fire (or need its `last` refreshed) over a signal someone
    /// actually wrote since the watcher's last observation, and every
    /// write to a watched signal marks its signal in the changed set
    /// (watched-only marking: unwatched signals have no watcher, so
    /// they never need an entry).  So the fired
    /// watchers are exactly the whole-table sweep's, and both walks go
    /// in ascending registration index: the bucket walk sorts the
    /// union of the changed signals' buckets; the table walk goes in
    /// table order.  Firing order among simultaneously-changed
    /// watchers is therefore identical to the unbucketed sweep,
    /// including when watchers on different signals change in the same
    /// settlement.
    ///
    /// The pass consumes the changed set at its start: it observes the
    /// watchers whose signals somebody wrote since the previous pass
    /// started, and any write landing while it runs (callbacks and
    /// their nested settlements) re-marks the set for the next pass.
    /// Two strategies, chosen per pass by whichever set is smaller:
    /// the bucket walk visits the changed signals' buckets (cost
    /// proportional to the watched signals that changed — the set
    /// carries watched signals only), collapsing
    /// the sweep toward watcher-fire cost on designs where few signals
    /// change per settlement; the table walk visits every watcher and
    /// pays the value compare (cost proportional to the table), which
    /// is the cheaper side on chain designs where most signals change
    /// every cycle but few have watchers.  Never worse than the old
    /// whole-table sweep, which paid a full value compare per table
    /// entry per pass.
    fn dispatch_pass(&mut self) {
        if self.changed_signal_count() < self.watchers.len() {
            // Bucket walk: visit only the changed signals' buckets, in
            // registration order (ascending index across buckets and
            // within each).
            self.dispatch_queue.clear();
            let mut queue = std::mem::take(&mut self.dispatch_queue);
            for signal_id in self.changed_signal_ids() {
                let index = signal_id.index();
                if let Some(bucket) = self.watcher_buckets.get(index) {
                    queue.extend_from_slice(bucket);
                }
            }
            self.dispatch_queue = queue;
            self.dispatch_queue.sort_unstable();
            // The queue now covers every watcher whose signal was
            // written since the previous pass started.  Consuming the
            // set here, before any callback runs, lets the pass's own
            // writes re-mark it for the next pass; the dispatch loop
            // ends when the set is empty, since a pass over an empty
            // set would observe nothing.
            self.clear_changed_since_dispatch();
            let mut table_len = self.watchers.len();
            let mut qi = 0;
            while qi < self.dispatch_queue.len() {
                let i = self.dispatch_queue[qi];
                qi += 1;
                self.dispatch_visit(i);
                // Watchers registered by callbacks during this pass are
                // visited at the end of this same pass: their indices
                // are larger than every queued one, so the queue stays
                // ascending and registration order is preserved —
                // exactly as the whole-table sweep did.
                while table_len < self.watchers.len() {
                    self.dispatch_queue.push(table_len);
                    table_len += 1;
                }
            }
        } else {
            // Table walk: every watcher, in table (registration) order
            // — the old whole-table sweep, which is cheaper than
            // queueing a changed set no smaller than the table.
            self.clear_changed_since_dispatch();
            let mut i = 0;
            while i < self.watchers.len() {
                self.dispatch_visit(i);
                i += 1;
            }
        }
    }

    /// Visit one watcher: compare the live value against its own
    /// last-seen value and fire if it observed a change.  Watchers
    /// registered by callbacks during the pass are appended to the
    /// table by the registration methods and picked up by the caller's
    /// walk; their `last` starts at the current value, so they first
    /// fire on a later transition.
    fn dispatch_visit(&mut self, i: usize) {
        // Decide (and observe) with immutable access, then act, so
        // the callback call below can take `&mut self`.
        let (fire, observed) = {
            let watcher = &self.watchers[i];
            let current = dispatch_ref!(self.inner, engine => engine.peek(watcher.signal));
            if *current == watcher.last {
                (false, None)
            } else {
                let fire = match watcher.kind {
                    WatchKind::Change => true,
                    WatchKind::ClockRising => {
                        !watcher.last.get_bit(0) && current.get_bit(0)
                    }
                };
                (fire, Some(current.clone()))
            }
        };
        if let Some(value) = observed {
            self.watchers[i].last = value;
        }
        if fire {
            let callback = self.watchers[i].callback.clone();
            (callback.borrow_mut())(self);
            if self.sched.is_shutdown_requested() {
                self.watchers[i].alive = false;
            }
        }
    }

    /// Rebuild `watcher_buckets` from the live table: called after the
    /// shutdown retain prunes dead watchers, so the buckets keep
    /// covering exactly the table, in registration order.
    fn rebuild_buckets(&mut self) {
        self.watcher_buckets.clear();
        // The prune may have dropped a signal's last watcher: rebuild
        // the watched flags alongside the buckets, so watched-only
        // marking keeps covering exactly the signals the live table
        // observes.  The changed set is empty here (the sweep loop
        // exits only when the set is empty), so no stale bit for a
        // newly-unwatched signal survives the rebuild.
        dispatch_mut!(self.inner, engine => engine.clear_watched());
        for (index, watcher) in self.watchers.iter().enumerate() {
            let idx = watcher.signal.index();
            if self.watcher_buckets.len() <= idx {
                self.watcher_buckets.resize(idx + 1, Vec::new());
            }
            self.watcher_buckets[idx].push(index);
            dispatch_mut!(self.inner, engine => engine.set_watched(watcher.signal, true));
        }
    }

    fn set_without_flow(&mut self, signal_id: SignalId, value: Value) {
        dispatch_mut!(self.inner, engine => engine.set_without_flow(signal_id, value));
    }

    pub fn get(&self, signal_id: SignalId) -> Value {
        dispatch_ref!(self.inner, engine => engine.get(signal_id))
    }

    /// True when some watched signal's stored value changed since the
    /// last sweep pass that observed everything (or since
    /// construction): the facade's sweep-skip gate, read from the
    /// engine's changed-set bookkeeping.  Exposed for the ported
    /// watcher-dispatch tests.
    fn changed_since_dispatch(&self) -> bool {
        dispatch_ref!(self.inner, engine => engine.changed_since_dispatch())
    }

    /// The number of watched signals with an actual write since the
    /// last dispatch pass started (or since the last clear): the
    /// dispatch loop's quiescence check and per-pass strategy switch.
    fn changed_signal_count(&self) -> usize {
        dispatch_ref!(self.inner, engine => engine.changed_signal_count())
    }

    fn changed_signal_ids(&self) -> impl Iterator<Item = SignalId> + '_ {
        dispatch_ref!(self.inner, engine => engine.changed_signal_ids())
    }

    fn clear_changed_since_dispatch(&mut self) {
        dispatch_mut!(self.inner, engine => engine.clear_changed_since_dispatch());
    }

    /// The interned [`TypeId`] of `signal_id`'s type — for constructing
    /// `Value`s of that type without cloning the `Type`.
    pub fn component_type_id(&self, signal_id: SignalId) -> TypeId {
        self.type_layout_index().type_id(&self.elaboration().component(signal_id).typ())
    }

    pub fn now(&self) -> u64 {
        self.sched.time_ps()
    }

    /// Advance simulated time by `ps` picoseconds without running the
    /// scheduler.  For the peek/poke tester: clocks are driven by
    /// manual `tick` edges instead of the `run` event loop (the only
    /// other thing that moves time), so each tick advances the clock
    /// explicitly to give the VCD trace — which stamps changes from
    /// `now()` — a progressing timeline.
    ///
    /// Must not be called with scheduled events pending (they would
    /// fire late); poke mode never schedules any.
    pub fn advance_time(&mut self, ps: u64) {
        self.sched.advance_ps(ps);
        if let Some(marker) = &self.time_marker {
            marker(self.sched.time_ps());
        }
    }

    /// Register the time-marker callback fired by [`Sim::advance_time`]
    /// (see the `time_marker` field).  The VCD dumper registers the
    /// only one; a second registration would replace it, but a `Sim`
    /// carries at most one trace.
    pub(crate) fn set_time_marker(&mut self, cb: Box<dyn Fn(u64)>) {
        self.time_marker = Some(cb);
    }

    /// Acquire a lock that defers circuit settlement.  While the lock is held,
    /// calls to `set()` accumulate signal changes without propagating them.
    /// When the lock is dropped, all accumulated changes are settled in a single
    /// `flow()` pass, and any watchers that fired are invoked.
    pub fn lock(&mut self) -> SimLock<'_> {
        SimLock { sim: self }
    }

    /// Request that the run loop stop dispatching after the current
    /// callback returns.  Safe to call from inside a callback; a watcher
    /// whose callback calls `finish()` is removed from the watcher table
    /// and does not survive into a subsequent `run()`.
    pub fn finish(&mut self) {
        self.sched.request_shutdown();
        // A structural change from the engines' point of view (the VM
        // engine invalidates its schedule caches here); the legacy
        // engine has nothing cached, so this is a no-op today.
        dispatch_mut!(self.inner, engine => engine.on_structural_change());
    }

    /// Drive `signal` as a free-running square wave described by `clock`.
    /// The signal is initialised low immediately and the first rising
    /// edge happens at `now + period_ps / 2`.
    ///
    /// The clock registers in the free-running clock table instead of
    /// as a scheduler event: the run loop advances time and toggles the
    /// signal directly whenever the next edge beats every queued user
    /// event, skipping the per-half-period heap push/pop and callback
    /// dispatch.  The edge keeps its old scheduler-event ordering
    /// against user `at`/`after` events: deadline `now + half`, FIFO
    /// sequence allocated after this settlement, exactly as the old
    /// `after(half)` registration.
    pub fn attach_clock(&mut self, signal: SignalId, clock: Clock) {
        let half = clock.period_ps / 2;
        {
            let typ_id = self.component_type_id(signal);
            let mut lock = self.lock();
            lock.set(signal, Value::from_bool(typ_id, false));
        }
        // Engine hook for the first clock attachment (the VM engine
        // computes its whole-design cycle plan here); a no-op on the
        // legacy engine.
        dispatch_mut!(self.inner, engine => engine.on_clock_attached(signal));
        let at_ps = self.sched.time_ps().saturating_add(half);
        let seq = self.sched.alloc_seq();
        self.clocks.push(ClockHandle {
            signal,
            half_period_ps: half,
            next: Some((at_ps, seq)),
        });
    }

    /// Drain the queue.  Order:
    ///   1. All `Event::StartOfSimulation` entries fire, in FIFO order.
    ///   2. While `finish()` has not been requested, repeatedly fire
    ///      whichever comes next, free-running clock edge or
    ///      `Event::AfterDelay` entry, compared in the heap's own
    ///      `(time, seq)` order — so a tie between an edge and a user
    ///      event keeps the sequence order the edge would have had as a
    ///      heap entry.  Each callback may itself register more entries
    ///      and trigger `set()`s; value-change watchers are dispatched
    ///      by the persistent watcher table at each settlement
    ///      (`Sim::unlock`), not through the queue.  An edge that beats
    ///      every queued event is stepped directly (`Sim::step_clock`),
    ///      without touching the heap.
    ///   3. All `Event::EndOfSimulation` entries fire, in FIFO order.
    /// Watchers (`on_change`/`on_clock`) persist across `run()` calls;
    /// only a watcher whose callback called `finish()` is removed.
    /// Free-running clocks likewise persist: an edge armed at the end
    /// of one `run()` is still pending when the next one starts, as it
    /// was as a leftover heap entry.
    pub fn run(&mut self) -> Result<(), SimError> {
        let start_callbacks = self.sched.drain_lifecycle(|e| matches!(e, Event::StartOfSimulation));
        for mut cb in start_callbacks {
            cb(self);
        }

        while !self.sched.is_shutdown_requested() {
            let event_next = self.sched.peek_next_timed();
            let clock_next = self.next_clock_edge();
            // The clock edge fires first when it beats the soonest
            // queued user event in the heap's own `(time, seq)` order.
            let clock_first = match (event_next, clock_next) {
                (Some((t, s)), Some((_, (edge_ps, edge_seq)))) => {
                    (edge_ps, edge_seq) < (t, s)
                }
                (None, Some(_)) => true,
                _ => false,
            };
            if clock_first {
                let (index, _) = clock_next.unwrap();
                self.step_clock(index);
            } else if let Some(mut cb) = self.sched.next_after_delay() {
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

    /// The soonest pending edge across all free-running clocks, as
    /// `(handle index, (time, seq))`, compared in the heap's `(time,
    /// seq)` order so same-time edges fire in registration order,
    /// exactly as they did as heap entries.  `None` when no clock has
    /// a pending edge (every armed handle, or none at all).
    fn next_clock_edge(&self) -> Option<(usize, (u64, u64))> {
        let mut best: Option<(usize, (u64, u64))> = None;
        for (i, handle) in self.clocks.iter().enumerate() {
            if let Some(next) = handle.next {
                best = match best {
                    None => Some((i, next)),
                    Some((_, best_next)) if next < best_next => Some((i, next)),
                    _ => best,
                };
            }
        }
        best
    }

    /// Step one free-running clock edge through exactly the work the
    /// old toggle callback did: advance `time_ps` to the edge, toggle
    /// the signal through the same commit path (`set_without_flow`,
    /// preserving rising-edge register transfer), settle exactly as a
    /// `SimLock` drop (`unlock`: flow plus the outermost dispatch
    /// sweep, `changed_since_dispatch` skip included), then re-arm the
    /// next edge without touching the heap or allocating.
    ///
    /// The handle is marked in-flight (no pending edge) across the
    /// settlement, mirroring the old heap state: the popped toggle
    /// entry was gone and the next one was only registered after the
    /// settlement, so a re-entrant `run()` from a settlement callback
    /// saw no pending edge for this clock.
    fn step_clock(&mut self, index: usize) {
        let (edge_ps, _seq) = self.clocks[index]
            .next
            .take()
            .expect("stepping a clock edge that is not armed");
        let signal = self.clocks[index].signal;
        let half = self.clocks[index].half_period_ps;
        self.sched.advance_to(edge_ps);
        // The old callback's toggle: read the live value, flip it,
        // commit through the lock's write path.  Toggling the live
        // value (not a stored phase) keeps the behavior when another
        // writer drove the signal between edges.
        let current = self.get(signal);
        let next = current.toggled();
        self.set_without_flow(signal, next);
        // The same settlement a `SimLock` drop runs.
        self.unlock();
        // Re-arm exactly as the old callback's `after(half)` did after
        // the settlement: deadline `edge + half` (a settlement cannot
        // move time), sequence allocated after any events the
        // settlement's callbacks registered.
        let at_ps = self.sched.time_ps().saturating_add(half);
        let seq = self.sched.alloc_seq();
        self.clocks[index].next = Some((at_ps, seq));
    }

}

////////////////////////////////////////////////////////////////////////////////
// Elaboration functions
////////////////////////////////////////////////////////////////////////////////
impl Sim {
    pub(super) fn elaboration(&self) -> Arc<Elaboration> {
        dispatch_ref!(self.inner, engine => engine.elaboration())
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

    /// The live words of memory `signal_id`, or `None` for any other
    /// component (surfaces memory words to `vir poke`).
    pub fn memory_words(&self, signal_id: SignalId) -> Option<std::sync::Arc<Vec<Value>>> {
        dispatch_ref!(self.inner, engine => engine.memory_words(signal_id))
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
    pub fn children_of(&self,
        parent: SignalId,
    ) -> Vec<(SignalId, BString)> {
        let parent_path = self.elaboration().component(parent).path().clone();
        self.children_of_path(parent_path.as_slice())
    }

    pub fn children_of_root(&self) -> Vec<(SignalId, BString)> {
        self.children_of_path(b"top")
    }

    /// Iterate over the direct child *components* of the module whose
    /// elaborated path prefix is `prefix` (e.g. `"top"` or
    /// `"top.foo"`).  Like [`Sim::children_of`], but addressed by path
    /// instead of by a component id — submodule instances are not
    /// components, so a nested module has no SignalId to pass.
    pub fn children_of_path(&self, prefix: &[u8]) -> Vec<(SignalId, BString)> {
        self.children_with_prefix(prefix)
    }

    /// The submodule *instances* directly under the module whose
    /// elaborated path prefix is `prefix` (e.g. `"top"` or
    /// `"top.foo"`).  Like [`Sim::children_of`], but addressed by path
    /// instead of by a component id — submodule instances are not
    /// components, so `cd`-style navigation needs this list to know
    /// which prefixes name modules at all.
    pub fn module_children(&self, prefix: &[u8]) -> Vec<BString> {
        let elaboration = self.elaboration();
        let mut out = Vec::new();
        for (_, module_prefix) in elaboration.modules() {
            let path: &[u8] = module_prefix.as_slice();
            if path.len() <= prefix.len() + 1 { continue; }
            if &path[..prefix.len()] != prefix { continue; }
            if path[prefix.len()] != b'.' { continue; }
            let rest = &path[prefix.len() + 1..];
            if rest.contains(&b'.') { continue; }
            out.push(BString::from(rest.to_vec()));
        }
        out
    }

    /// True when `prefix` names a module in the elaboration — the top
    /// (`"top"`) or any submodule instance path.
    pub fn has_module_prefix(&self, prefix: &[u8]) -> bool {
        let elaboration = self.elaboration();
        elaboration.modules().iter().any(|(_, module_prefix)| module_prefix.as_slice() == prefix)
    }

    /// The moddef symbol of the module instance whose elaborated path
    /// prefix is `prefix` (e.g. `SymbolId(Adder)` for `"top.add"`).
    /// For the top module itself (`"top"`) this is the top moddef.
    pub fn module_symbol(&self, prefix: &[u8]) -> Option<SymbolId> {
        let elaboration = self.elaboration();
        elaboration
            .modules()
            .iter()
            .find(|(_, module_prefix)| module_prefix.as_slice() == prefix)
            .map(|(moddef, _)| *moddef)
    }

    /// The socket instances directly under the module whose elaborated
    /// path prefix is `prefix`, one `(leaf name, socketdef symbol)`
    /// pair per instance (e.g. `("mem", SymbolId(Mem))` under
    /// `"top.core"`).
    pub fn socket_children(&self, prefix: &[u8]) -> Vec<(BString, SymbolId)> {
        let elaboration = self.elaboration();
        let mut out = Vec::new();
        for socket in elaboration.sockets() {
            let path = socket.prefix();
            if path.len() <= prefix.len() + 1 { continue; }
            if &path[..prefix.len()] != prefix { continue; }
            if path[prefix.len()] != b'.' { continue; }
            let rest = &path[prefix.len() + 1..];
            if rest.contains(&b'.') { continue; }
            out.push((BString::from(rest.to_vec()), socket.socketdef()));
        }
        out
    }

    /// The module instances in the elaboration: `(moddef symbol, path
    /// prefix)` pairs, in elaboration order (`"top"` first, then
    /// submodules as discovered).  Backing for `show`'s per-module
    /// grouping.
    pub fn modules(&self) -> Vec<(SymbolId, BString)> {
        self.elaboration().modules().to_vec()
    }

    /// Every elaborated component as `(id, path)`, in elaboration
    /// order.  Unlike [`Sim::children_of`], this is the exhaustive flat
    /// list: module instances are not themselves components, so a
    /// `children_of` walk cannot reach components nested inside one
    /// (e.g. `top.gcd.state`); this can.
    pub fn components(&self) -> Vec<(SignalId, BString)> {
        self.elaboration()
            .components()
            .iter()
            .map(|comp| (comp.id(), comp.path().clone()))
            .collect()
    }

    /// The socket instances in the elaboration, each with its
    /// fully-qualified channel prefix (e.g. `"top.mem"`) and socketdef
    /// symbol.  Backing for `show`'s per-socket grouping.
    pub fn sockets(&self) -> Vec<(SymbolId, BString)> {
        self.elaboration()
            .sockets()
            .iter()
            .map(|socket| (socket.socketdef(), BString::from(socket.prefix())))
            .collect()
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
    /// Adds a persistent entry to the watcher table: no re-registration
    /// happens — the entry is swept in place at each settlement, firing
    /// (in registration order, after `flow()`) whenever the signal's
    /// value differs from the last value *this watcher* observed.
    ///
    /// If `cb` calls `sim.finish()`, the watcher is removed from the
    /// table: it does not survive into a subsequent `run()`.
    pub fn on_change(&mut self, signal: SignalId, cb: Callback) {
        let last = self.get(signal);
        self.watchers.push(Watcher {
            signal,
            kind: WatchKind::Change,
            last,
            alive: true,
            callback: Rc::new(RefCell::new(cb)),
        });
        let index = self.watchers.len() - 1;
        let idx = signal.index();
        if self.watcher_buckets.len() <= idx {
            self.watcher_buckets.resize(idx + 1, Vec::new());
        }
        self.watcher_buckets[idx].push(index);
        // Watched-only marking: from here on, writes to this signal pay
        // the changed-set bookkeeping (and this watcher can fire).  A
        // write before registration could not have been observed anyway:
        // `last` starts at the live value.
        dispatch_mut!(self.inner, engine => {
            engine.set_watched(signal, true);
            engine.on_structural_change();
        });
    }

    /// Register `cb` to fire every time `clock`'s value has a rising edge
    /// (0→1 transition on a `Bit` signal).  Adds a persistent entry to
    /// the watcher table: no re-registration happens — the entry is
    /// swept in place at each settlement, in registration order.
    ///
    /// The watcher tracks the last value *it* observed (per-watcher
    /// state, not shared circuit state), so each rising edge is observed
    /// exactly once regardless of settlement ordering.
    ///
    /// If `cb` calls `sim.finish()`, the watcher is removed from the
    /// table: it does not survive into a subsequent `run()`.
    pub fn on_clock(&mut self, clock: SignalId, cb: Callback) {
        let last = self.get(clock);
        self.watchers.push(Watcher {
            signal: clock,
            kind: WatchKind::ClockRising,
            last,
            alive: true,
            callback: Rc::new(RefCell::new(cb)),
        });
        let index = self.watchers.len() - 1;
        let idx = clock.index();
        if self.watcher_buckets.len() <= idx {
            self.watcher_buckets.resize(idx + 1, Vec::new());
        }
        self.watcher_buckets[idx].push(index);
        // Watched-only marking, as in `on_change`.
        dispatch_mut!(self.inner, engine => {
            engine.set_watched(clock, true);
            engine.on_structural_change();
        });
    }

    /// Register a callback by raw [`EventCallback`] — the uniform API
    /// behind `at`/`after`/`at_start`/`at_end`/`on_change`.  An
    /// `Event::ValueChange` entry is routed into the persistent watcher
    /// table (equivalent to `on_change`); every other event joins the
    /// scheduler queue.
    pub fn register(&mut self, event_cb: EventCallback) {
        match event_cb.event {
            Event::ValueChange { signal } => {
                if let Some(cb) = event_cb.callback {
                    self.on_change(signal, cb);
                }
            }
            _ => self.sched.register(event_cb),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// VM-only inspection
////////////////////////////////////////////////////////////////////////////////
impl Sim {
    /// Disassemble every compiled driver program in human-readable
    /// form: one section per driven component, branch targets as
    /// labels, signal references by path.  The format `vir run
    /// --bytecode` prints and `vm/INSTRUCTIONS.md` documents.
    ///
    /// VM-only: the legacy engine has no compiled bytecode.
    pub fn dump_bytecode(&self) -> String {
        dispatch_ref!(self.inner, engine => engine.dump_bytecode())
    }

    /// The whole-design cycle plan verdict as a machine-readable
    /// string: `"qualified"` or `"disqualified(reason)"`, or
    /// `"pending"` when no clock has been attached yet.  For tests and
    /// `simprof`.  VM-only: `"legacy"` on the legacy engine, which has
    /// no cycle plan.
    pub fn cycle_plan_verdict(&self) -> String {
        dispatch_ref!(self.inner, engine => engine.cycle_plan_verdict())
    }

    /// The cycle plan verdict with any disqualification decoded to
    /// full signal names (the offending component's elaborated path),
    /// for `simprof` and load-time diagnostics.  `"pending"` before
    /// the first `attach_clock`.  VM-only: `"legacy"` on the legacy
    /// engine, which has no cycle plan.
    pub fn cycle_plan_diagnostic(&self) -> String {
        dispatch_ref!(self.inner, engine => engine.cycle_plan_diagnostic())
    }

    /// The whole-design cycle plan verdict, once the primary clock has
    /// been attached: the static schedule data for a qualified design,
    /// or the precise disqualification reason.  `None` before the
    /// first `attach_clock` (and on the legacy engine, which has no
    /// cycle plan).  Internal: tests inspect it; a later phase
    /// consumes it.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(in crate::sim) fn cycle_plan(&self) -> Option<&crate::sim::vm::CyclePlanResult> {
        dispatch_ref!(self.inner, engine => engine.cycle_plan())
    }

    /// Whether the engine's program carries a static topological
    /// schedule (the VM engine's stage-A schedule; the legacy engine
    /// never builds one).  For the ported schedule/epoch tests.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(in crate::sim) fn has_topo_schedule(&self) -> bool {
        dispatch_ref!(self.inner, engine => engine.has_topo_schedule())
    }
}

/// Intentionally-empty hook for tracing register transfers when
/// debugging; params kept for call-site readability.
pub fn debug_transfer(_id: crate::analysis::elaboration::SignalId, _v: &Value) {
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::DiagnosticPayload;
    use crate::sim::vm;
    use crate::util::{check_db, db_from_dir_with_lib};
    use std::cell::Cell;
    use std::path::Path;
    use std::sync::atomic::{AtomicU64, Ordering};

    /// Build a settled `Sim` for the `tests/sim/gcd` design (its source
    /// file is `top.vir`, so the ModDef symbol path is `top::Top`).
    fn gcd_sim(engine: Engine) -> Sim {
        let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
        let source = manifest.join("../tests/sim/gcd/src");
        let lib = manifest.join("../lib");
        let db = db_from_dir_with_lib(&source, &lib);
        if let Err(diagnostics) = check_db(&db) {
            for diag in diagnostics.iter() {
                eprintln!("{diag:?}");
            }
            panic!("gcd test design failed to build");
        }
        let symboltable = db.get_symboltable();
        let top = symboltable.resolve(b"top::Top".into()).unwrap();
        Sim::with_engine(&db, top.id(), InitStrategy::Seeded(42), engine)
    }

    /// The sweep-skip predicate, witnessed by callback counts: a real
    /// value change fires a `Change` watcher exactly once, and a
    /// lock/unlock that writes the same value (a settlement whose
    /// `changed_since_dispatch` flag stays down, so the sweep is
    /// skipped entirely) fires nothing.
    #[test]
    fn watcher_fires_on_change_and_skips_quiescent_settlements() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let fire = sim.signal("top.fire");
            let fire_typ = sim.component_type_id(fire);
            let initial_high = sim.get(fire).get_bit(0);

            let fires = Rc::new(RefCell::new(0u32));
            {
                let count = fires.clone();
                sim.on_change(fire, Box::new(move |_sim| {
                    *count.borrow_mut() += 1;
                }));
            }

            // A real change: drive fire to the opposite of its seeded
            // initial value.
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !initial_high));
            }
            assert_eq!(*fires.borrow(), 1, "a real value change must fire the watcher");

            // A settlement with no actual write: the sweep is skipped, so
            // the watcher cannot fire again.
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !initial_high));
            }
            assert_eq!(*fires.borrow(), 1, "a same-value write must not fire the watcher");

            // A second real change fires again; repeating it does not.
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, initial_high));
            }
            assert_eq!(*fires.borrow(), 2, "a second real change must fire again");
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, initial_high));
            }
            assert_eq!(*fires.borrow(), 2, "a repeated same-value write must not fire");
    }
    }

    /// `ClockRising` watchers under the skip: the first settlement
    /// drives the clock low (no fire: no change or a falling edge),
    /// the second is a real rising edge (one fire), the third writes
    /// the same high value (sweep skipped, no fire), the fourth is a
    /// falling edge (observed but no fire), the fifth is a second
    /// rising edge (one more fire).  Rising edges also transfer the
    /// gcd registers, exercising `transfer`'s flag updates.
    #[test]
    fn on_clock_watcher_fires_only_on_rising_edges() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let clock = sim.signal("top.clock");
            let clock_typ = sim.component_type_id(clock);

            let rises = Rc::new(RefCell::new(0u32));
            {
                let count = rises.clone();
                sim.on_clock(clock, Box::new(move |_sim| {
                    *count.borrow_mut() += 1;
                }));
            }

            let mut settle = |sim: &mut Sim, high: bool| {
                let mut lock = sim.lock();
                lock.set(clock, Value::from_bool(clock_typ, high));
            };

            settle(&mut sim, false);
            assert_eq!(*rises.borrow(), 0, "driving low must not fire an on_clock watcher");

            settle(&mut sim, true);
            assert_eq!(*rises.borrow(), 1, "a rising edge must fire exactly once");

            settle(&mut sim, true);
            assert_eq!(*rises.borrow(), 1, "a same-value write must not fire again");

            settle(&mut sim, false);
            assert_eq!(*rises.borrow(), 1, "a falling edge must not fire");

            settle(&mut sim, true);
            assert_eq!(*rises.borrow(), 2, "a second rising edge must fire once more");
    }
    }

    /// A watcher callback that drives a signal during the sweep: the
    /// fire watcher fires on the settlement's change and drives `x`,
    /// whose own watcher fires within the same unlock (the dispatching
    /// loop re-sweeps after the nested flow-only unlock re-settled the
    /// driven write).  A later quiescent unlock (same fire value) fires
    /// neither: the flag was cleared by the quiescent pass and the
    /// no-op settlement skips the sweep.
    #[test]
    fn watcher_cascade_settles_within_one_unlock() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let fire = sim.signal("top.fire");
            let x = sim.signal("top.x");
            let fire_typ = sim.component_type_id(fire);
            let x_typ = sim.component_type_id(x);
            let initial_high = sim.get(fire).get_bit(0);

            // Pre-drive x to a known value so the cascade write below is
            // guaranteed to be a real change regardless of the seeded init.
            {
                let mut lock = sim.lock();
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, 5)));
            }

            let fire_fires = Rc::new(RefCell::new(0u32));
            let x_fires = Rc::new(RefCell::new(0u32));
            {
                let count = fire_fires.clone();
                sim.on_change(fire, Box::new(move |sim| {
                    *count.borrow_mut() += 1;
                    let mut lock = sim.lock();
                    lock.set(x, Value::new(x_typ, BitPat::from_u64(8, 6)));
                }));
            }
            {
                let count = x_fires.clone();
                sim.on_change(x, Box::new(move |_sim| {
                    *count.borrow_mut() += 1;
                }));
            }

            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !initial_high));
            }
            assert_eq!(*fire_fires.borrow(), 1, "the fire change must fire its watcher");
            assert_eq!(*x_fires.borrow(), 1, "the cascade-driven x change must fire too");

            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !initial_high));
            }
            assert_eq!(*fire_fires.borrow(), 1, "a quiescent settlement must not re-fire");
            assert_eq!(*x_fires.borrow(), 1, "a quiescent settlement must not re-fire");
    }
    }

    /// Bucketed dispatch keeps registration order across different
    /// signals: two watchers observing different signals, both changed
    /// by the same settlement, fire in registration order — for both
    /// registration orders, so the test pins order to registration,
    /// not to signal identity or bucket visit order.
    #[test]
    fn watchers_on_different_signals_fire_in_registration_order() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let run_phase = |register_fire_first: bool| -> Vec<&'static str> {
                let mut sim = gcd_sim(engine);
                let fire = sim.signal("top.fire");
                let x = sim.signal("top.x");
                let fire_typ = sim.component_type_id(fire);
                let x_typ = sim.component_type_id(x);

                let order: Rc<RefCell<Vec<&'static str>>> = Rc::new(RefCell::new(Vec::new()));
                let fire_watcher = {
                    let order = order.clone();
                    Box::new(move |_sim: &mut Sim| {
                        order.borrow_mut().push("fire");
                    })
                };
                let x_watcher = {
                    let order = order.clone();
                    Box::new(move |_sim: &mut Sim| {
                        order.borrow_mut().push("x");
                    })
                };
                if register_fire_first {
                    sim.on_change(fire, fire_watcher);
                    sim.on_change(x, x_watcher);
                } else {
                    sim.on_change(x, x_watcher);
                    sim.on_change(fire, fire_watcher);
                }

                // One settlement changes both signals: a single dispatch
                // pass fires both, in registration order.
                let fire_val = sim.get(fire).get_bit(0);
                let x_val = sim.get(x).to_u64();
                {
                    let mut lock = sim.lock();
                    lock.set(fire, Value::from_bool(fire_typ, !fire_val));
                    lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_val ^ 0xFF)));
                }

                // A second settlement toggles both again: the order holds.
                let fire_val = sim.get(fire).get_bit(0);
                let x_val = sim.get(x).to_u64();
                {
                    let mut lock = sim.lock();
                    lock.set(fire, Value::from_bool(fire_typ, !fire_val));
                    lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_val ^ 0xFF)));
                }

                order.borrow().clone()
            };

            assert_eq!(
                run_phase(true),
                vec!["fire", "x", "fire", "x"],
                "fire registered first: the pass fires in registration order"
            );
            assert_eq!(
                run_phase(false),
                vec!["x", "fire", "x", "fire"],
                "x registered first: the pass fires in registration order"
            );
    }
    }

    /// A `Change` watcher on the clock signal fires on every value
    /// change, including falling edges: the bucketed sweep visits the
    /// clock's bucket on any write to it, so the opposite-direction
    /// edge (which a `ClockRising` watcher ignores) still fires a
    /// `Change` watcher watching the same signal.
    #[test]
    fn on_change_watcher_on_clock_fires_on_falling_edges() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let clock = sim.signal("top.clock");
            let clock_typ = sim.component_type_id(clock);

            let fires = Rc::new(RefCell::new(0u32));
            {
                let count = fires.clone();
                sim.on_change(clock, Box::new(move |_sim| {
                    *count.borrow_mut() += 1;
                }));
            }

            let initial_high = sim.get(clock).get_bit(0);
            let mut settle = |sim: &mut Sim, high: bool| {
                let mut lock = sim.lock();
                lock.set(clock, Value::from_bool(clock_typ, high));
            };

            // Drive to the opposite of the seeded initial value: one
            // change, one fire (the edge direction depends on the seed).
            settle(&mut sim, !initial_high);
            assert_eq!(*fires.borrow(), 1, "the first change must fire");

            // Drive back: the opposite-direction edge (falling when the
            // initial value was high) fires a Change watcher too.
            settle(&mut sim, initial_high);
            assert_eq!(
                *fires.borrow(),
                2,
                "the opposite-direction edge must fire a Change watcher"
            );

            // A same-value write is not a change: no fire.
            settle(&mut sim, initial_high);
            assert_eq!(*fires.borrow(), 2, "a same-value write must not fire");
    }
    }

    /// Unregister edge case: a watcher whose callback calls `finish()`
    /// is pruned from the table (and its bucket rebuilt) at the end of
    /// the unlock, while watchers on other signals survive it and keep
    /// firing.  The surviving watcher's own later fire then dies to the
    /// post-shutdown rule (watchers firing after the shutdown request
    /// do not persist), so a final settlement of its signal fires
    /// nothing.
    #[test]
    fn finish_prunes_only_the_calling_watcher_and_rebuilds_buckets() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let fire = sim.signal("top.fire");
            let x = sim.signal("top.x");
            let fire_typ = sim.component_type_id(fire);
            let x_typ = sim.component_type_id(x);

            let x_fires = Rc::new(RefCell::new(0u32));
            {
                let count = x_fires.clone();
                sim.on_change(x, Box::new(move |_sim| {
                    *count.borrow_mut() += 1;
                }));
            }
            {
                sim.on_change(fire, Box::new(move |sim| {
                    sim.finish();
                }));
            }

            // The fire watcher fires, calls finish(), and is pruned; the
            // x watcher (registered earlier, on another signal) survives
            // the retain and the bucket rebuild.
            let fire_initial = sim.get(fire).get_bit(0);
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !fire_initial));
            }
            let x_initial = sim.get(x).to_u64();
            {
                let mut lock = sim.lock();
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_initial ^ 0xFF)));
            }
            assert_eq!(
                *x_fires.borrow(),
                1,
                "the surviving watcher must still fire after the finish() prune and bucket rebuild"
            );

            // The survivor's next fire runs after the shutdown request, so
            // it dies (existing post-finish semantics): no third fire.
            let x_now = sim.get(x).to_u64();
            {
                let mut lock = sim.lock();
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_now ^ 0xFF)));
            }
            assert_eq!(
                *x_fires.borrow(),
                1,
                "a watcher firing after the shutdown request must not persist"
            );
    }
    }

    /// Watched-only marking must not lose fires: a watcher on `x`
    /// still fires on every one of `x`'s transitions even though each
    /// settlement also writes `fire`, an unwatched signal whose writes
    /// no longer touch the changed set.  A watched signal's write is
    /// marked regardless of what else the settlement does, so the
    /// watcher's fires are exactly the whole-table sweep's.
    #[test]
    fn watcher_fires_alongside_unwatched_signal_writes() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let fire = sim.signal("top.fire");
            let x = sim.signal("top.x");
            let fire_typ = sim.component_type_id(fire);
            let x_typ = sim.component_type_id(x);

            let fires = Rc::new(RefCell::new(0u32));
            {
                let count = fires.clone();
                sim.on_change(x, Box::new(move |_sim| {
                    *count.borrow_mut() += 1;
                }));
            }

            // Each settlement writes both signals: `fire` (unwatched) and
            // `x` (watched).  The watcher fires once per settlement, once
            // per real change of `x` — the unwatched signal's write in the
            // same settlement neither suppresses nor delays it.
            for i in 0..3u32 {
                let x_val = sim.get(x).to_u64();
                let fire_val = sim.get(fire).get_bit(0);
                {
                    let mut lock = sim.lock();
                    lock.set(fire, Value::from_bool(fire_typ, !fire_val));
                    lock.set(x, Value::new(x_typ, BitPat::from_u64(8, x_val ^ 0xFF)));
                }
                assert_eq!(*fires.borrow(), i + 1, "settlement {} must fire the x watcher once", i);
            }
    }
    }

    /// Zero-watcher designs skip the changed-set bookkeeping entirely:
    /// with no watcher anywhere, every write leaves the changed set
    /// empty (the sweep-skip gate stays down and the dispatch loop
    /// never runs), while the circuit still settles normally.
    #[test]
    fn zero_watcher_design_leaves_the_changed_set_empty() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let fire = sim.signal("top.fire");
            let fire_typ = sim.component_type_id(fire);

            // A settlement with no watcher anywhere: the circuit still
            // settles, and the changed set stays empty.
            let initial = sim.get(fire).get_bit(0);
            {
                let mut lock = sim.lock();
                lock.set(fire, Value::from_bool(fire_typ, !initial));
            }
            assert_eq!(sim.get(fire).get_bit(0), !initial, "the write must land");
            assert!(
                !sim.changed_since_dispatch(),
                "no watcher: a write must not raise the sweep-skip gate"
            );
            assert_eq!(
                sim.changed_signal_count(),
                0,
                "no watcher: a write must not touch the changed set"
            );
    }
    }

    /// The narrow commit path end to end: `top.valid`'s `on_change`
    /// watcher fires once per actual value transition and never on a
    /// same-value re-commit, even though the narrow drivers are
    /// re-evaluated every cycle.  Pins the watched-signal change
    /// protocol across a full clocked gcd run.
    #[test]
    fn narrow_bits_commit_path_runs_gcd_to_completion() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let clock = sim.signal("top.clock");
            let clock_typ = sim.component_type_id(clock);
            let reset = sim.signal("top.reset");
            let reset_typ = sim.component_type_id(reset);
            let fire = sim.signal("top.fire");
            let fire_typ = sim.component_type_id(fire);
            let x = sim.signal("top.x");
            let x_typ = sim.component_type_id(x);
            let y = sim.signal("top.y");
            let y_typ = sim.component_type_id(y);
            let result = sim.signal("top.result");
            let valid = sim.signal("top.valid");

            let mut settle = |sim: &mut Sim, high: bool| {
                let mut lock = sim.lock();
                lock.set(clock, Value::from_bool(clock_typ, high));
            };

            // Drive the inputs: gcd(9, 6) = 3, with reset held for the
            // first edge so the seeded-random register state starts at
            // `Idle`.
            {
                let mut lock = sim.lock();
                lock.set(reset, Value::from_bool(reset_typ, true));
                lock.set(fire, Value::from_bool(fire_typ, true));
                lock.set(x, Value::new(x_typ, BitPat::from_u64(8, 9)));
                lock.set(y, Value::new(y_typ, BitPat::from_u64(8, 6)));
            }

            // First rising edge: reset transfers `state` to `Idle`, so
            // `result`/`valid` are 0/false from here on.
            settle(&mut sim, true);
            assert_eq!(sim.get(result).to_u64(), 0);
            assert!(!sim.get(valid).get_bit(0));

            // Deassert reset: the pending next-value recomputes to
            // `Running(9, 6)` (Idle + fire), transferred at the next edge.
            {
                let mut lock = sim.lock();
                lock.set(reset, Value::from_bool(reset_typ, false));
            }

            // Watch the alias copy of the narrow `valid` driver's output:
            // the driver is re-evaluated and re-committed every cycle, and
            // only the two real transitions (false->true at Done, true->false
            // back at Idle) may fire.
            let fires = Rc::new(RefCell::new(0u32));
            {
                let count = fires.clone();
                sim.on_change(valid, Box::new(move |_sim| {
                    *count.borrow_mut() += 1;
                }));
            }

            // Rising-edge sequence for `state`: Running(9,6), Running(3,6),
            // Running(3,3), Running(0,3), Running(3,0), Done(3), Idle.  The
            // register driver re-evaluates on the clock edges, so the first
            // edge after deasserting reset still commits the `Idle` pending.
            for _ in 0..6 {
                settle(&mut sim, true);
                settle(&mut sim, false);
            }
            // Eighth rising edge after Idle: `Done(3)`.
            settle(&mut sim, true);
            assert_eq!(sim.get(result).to_u64(), 3, "gcd(9, 6) = 3");
            assert!(sim.get(valid).get_bit(0));
            assert_eq!(*fires.borrow(), 1, "the false->true transition fires once");
            settle(&mut sim, false);
            // Ninth: `Done` returns to `Idle`.
            settle(&mut sim, true);
            assert_eq!(sim.get(result).to_u64(), 0);
            assert!(!sim.get(valid).get_bit(0));
            assert_eq!(*fires.borrow(), 2, "the true->false transition fires once");
    }
    }

    /// A `Write` sink over a shared buffer, so a test can attach the
    /// VCD dumper and inspect the bytes it produced.
    struct SharedSink(Rc<RefCell<Vec<u8>>>);

    impl std::io::Write for SharedSink {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.borrow_mut().extend_from_slice(buf);
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    /// Event interleaving between the free-running clock and user
    /// `after` events: exact (tag, time) order of every dispatched
    /// callback, captured from the pre-fast-path engine (where each
    /// clock edge was a scheduler heap entry) on the same design and
    /// seed.  Covers:
    ///
    /// - an `after` strictly between two edges (7500) fires there;
    /// - an `after` registered later (7500) for an edge's exact time
    ///   (10000) fires after that edge: the edge's heap entry was
    ///   registered at 5000, so it holds the lower seq;
    /// - an `after` registered from inside an edge's settlement (at
    ///   5000) for the next edge's exact time (15000) fires BEFORE
    ///   that edge: it holds the lower seq, because the clock re-arms
    ///   only after its settlement;
    /// - `finish()` from a rising-edge watcher stops the run cleanly
    ///   with time parked at the last edge (15000).
    #[test]
    fn clock_and_after_events_interleave_like_the_heap_engine() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let clock = sim.signal("top.clock");
            sim.attach_clock(clock, Clock::with_period_ps(10_000));

            let log: Rc<RefCell<Vec<String>>> = Rc::new(RefCell::new(Vec::new()));
            let push = |log: &Rc<RefCell<Vec<String>>>, tag: &str, t: u64| {
                log.borrow_mut().push(format!("{tag} {t}"));
            };

            {
                let log_cb = log.clone();
                let armed = Rc::new(Cell::new(false));
                let armed_cb = armed.clone();
                sim.on_clock(clock, Box::new(move |sim| {
                    push(&log_cb, "clock-rise", sim.now());
                    if !armed_cb.get() {
                        armed_cb.set(true);
                        let log2 = log_cb.clone();
                        sim.after(10_000, Box::new(move |sim| {
                            push(&log2, "watcher-after", sim.now());
                        }));
                    }
                }));
            }
            {
                let log_cb = log.clone();
                sim.at_start(Box::new(move |sim| {
                    push(&log_cb, "start", sim.now());
                }));
            }
            {
                let log_cb = log.clone();
                sim.at_start(Box::new(move |sim| {
                    let log2 = log_cb.clone();
                    sim.after(7_500, Box::new(move |sim| {
                        push(&log2, "between", sim.now());
                        let log3 = log2.clone();
                        sim.after(2_600, Box::new(move |sim| {
                            push(&log3, "late", sim.now());
                        }));
                        let log3 = log2.clone();
                        sim.after(2_500, Box::new(move |sim| {
                            push(&log3, "tie-after", sim.now());
                        }));
                    }));
                }));
            }
            {
                let rises: Rc<Cell<u64>> = Rc::new(Cell::new(0));
                let rises_cb = rises.clone();
                sim.on_clock(clock, Box::new(move |sim| {
                    let n = rises_cb.get() + 1;
                    rises_cb.set(n);
                    if n >= 2 {
                        sim.finish();
                    }
                }));
            }

            sim.run().unwrap();
            let log = log.borrow();
            let expected = [
                "start 0",
                "clock-rise 5000",
                "between 7500",
                "tie-after 10000",
                "late 10100",
                "watcher-after 15000",
                "clock-rise 15000",
            ];
            for (got, want) in log.iter().zip(expected.iter()) {
                assert_eq!(got, want, "dispatch order diverged from the heap engine");
            }
            assert_eq!(log.len(), expected.len(), "unexpected extra dispatches");
            assert_eq!(sim.now(), 15_000, "finish() parks time at the last edge");
    }
    }

    /// The VCD dump of a clocked run must be byte-identical to the
    /// pre-fast-path engine's, timestamp lines included: the fast path
    /// advances `time_ps` exactly where the heap-event engine did.
    /// Golden captured from the pre-change engine on the gcd design,
    /// seed 42, 16 rising edges, period 10000 ps.
    #[test]
    fn vcd_trace_is_byte_identical_to_the_heap_engine() {
    for engine in [Engine::Vm, Engine::Legacy] {
            let mut sim = gcd_sim(engine);
            let clock = sim.signal("top.clock");
            let sink = SharedSink(Rc::new(RefCell::new(Vec::new())));
            let sink_ref = sink.0.clone();
            sim.attach_vcd(sink);
            sim.attach_clock(clock, Clock::with_period_ps(10_000));

            let rises: Rc<Cell<u64>> = Rc::new(Cell::new(0));
            {
                let rises_cb = rises.clone();
                sim.on_clock(clock, Box::new(move |sim| {
                    let n = rises_cb.get() + 1;
                    rises_cb.set(n);
                    if n >= 16 {
                        sim.finish();
                    }
                }));
            }
            sim.run().unwrap();
            assert_eq!(rises.get(), 16);

            let golden_path =
                Path::new(env!("CARGO_MANIFEST_DIR")).join("../sim-bench/golden/gcd.vcd");
            let golden = std::fs::read(&golden_path).unwrap_or_else(|e| {
                panic!(
                    "cannot read VCD golden {:?}: {e} (run the pre-change capture first)",
                    golden_path
                )
            });
            assert_eq!(
                sink_ref.borrow().as_slice(),
                golden.as_slice(),
                "VCD output diverged from the heap-event engine"
            );
    }
    }

    /// Materialize a generated chain design (the `sim-bench/gen.py`
    /// shape: `Word[32]` registers chained with `+`, every register
    /// changing on every cycle) in a fresh temp directory, and build a
    /// seeded `Sim` over it.  The per-test directory name carries a
    /// process-unique counter so parallel test threads never collide.
    /// VM-only: the schedule/cycle-plan tests inspect VM internals.
    fn chain_sim(stages: usize) -> Sim {
        static SEQ: AtomicU64 = AtomicU64::new(0);
        let dir = std::env::temp_dir().join(format!(
            "virdant-chain-test-{}-{}-{stages}",
            std::process::id(),
            SEQ.fetch_add(1, Ordering::Relaxed),
        ));
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();
        let mut regs = String::new();
        for i in 0..stages {
            if i == 0 {
                regs.push_str("    reg r0 : Word[32] on clock {\n        it <= it + 1w32\n    }\n");
            } else {
                regs.push_str(&format!(
                    "    reg r{i} : Word[32] on clock {{\n        it <= it + r{} + 1w32\n    }}\n",
                    i - 1
                ));
            }
        }
        let top_vir = format!(
            "//! Generated test bench: {stages}-stage register chain.\n\npub mod Top for virt::Virt {{\n    incoming clock : Clock\n\n    outgoing tx : Bit\n\n{regs}    tx := r0[0]\n}}\n"
        );
        std::fs::write(src_dir.join("top.vir"), top_vir).unwrap();
        std::fs::write(
            dir.join("virdant.toml"),
            "[project]\nname = \"chain\"\n\n[prog]\ntop = \"top::Top\"\n",
        )
        .unwrap();

        let lib = Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib");
        let db = db_from_dir_with_lib(&src_dir, &lib);
        if let Err(diagnostics) = check_db(&db) {
            for diag in diagnostics.iter() {
                eprintln!("{diag:?}");
            }
            panic!("chain test design failed to build");
        }
        let symboltable = db.get_symboltable();
        let top = symboltable.resolve(b"top::Top".into()).unwrap();
        Sim::new(&db, top.id(), InitStrategy::Seeded(42))
    }

    /// The static combinational schedule engages on acyclic designs
    /// (both a real fixture and a generated chain) and is absent only
    /// when the driver graph cannot be ordered; the epoch machinery in
    /// `flow` is live exactly then.  VM-only: the schedule is the VM
    /// engine's stage-A artifact.
    #[test]
    fn static_schedule_engages_on_acyclic_designs() {
        let sim = gcd_sim(Engine::Vm);
        assert!(
            sim.has_topo_schedule(),
            "gcd's driver graph is acyclic, so the schedule must engage"
        );
        let sim = chain_sim(4);
        assert!(
            sim.has_topo_schedule(),
            "the chain's driver graph is acyclic, so the schedule must engage"
        );
    }

    /// Build a `Sim` for a design whose source is `source` (one `top::Top`
    /// module, same `virt::Virt` library the other fixtures use), attach
    /// the primary clock, and return it.  The temp directory is removed
    /// as soon as the `Db` is built (the `Db` is self-contained), so the
    /// tests do not accumulate files in `/tmp`.
    fn design_sim(stem: &str, source: &str) -> Sim {
        static SEQ: AtomicU64 = AtomicU64::new(0);
        let dir = std::env::temp_dir().join(format!(
            "virdant-cycle-plan-test-{}-{}-{stem}",
            std::process::id(),
            SEQ.fetch_add(1, Ordering::Relaxed),
        ));
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();
        std::fs::write(src_dir.join("top.vir"), source).unwrap();
        std::fs::write(
            dir.join("virdant.toml"),
            "[project]\nname = \"cycletest\"\n\n[prog]\ntop = \"top::Top\"\n",
        )
        .unwrap();
        let lib = Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib");
        let db = db_from_dir_with_lib(&src_dir, &lib);
        std::fs::remove_dir_all(&dir).unwrap();
        // The `cyclic` fixture is the qualification checker's clause-2
        // case, so its design intentionally carries a CombinationalLoop
        // diagnostic; any other diagnostic is a broken fixture.
        let diagnostics = match check_db(&db) {
            Ok(diagnostics) => diagnostics,
            Err(diagnostics) => diagnostics,
        };
        for diag in diagnostics.iter() {
            if matches!(diag.payload, DiagnosticPayload::CombinationalLoop(_)) {
                continue;
            }
            eprintln!("{diag:?}");
            panic!("cycle-plan test design `{stem}` failed to build");
        }
        let symboltable = db.get_symboltable();
        let top = symboltable.resolve(b"top::Top".into()).unwrap();
        let mut sim = Sim::new(&db, top.id(), InitStrategy::Seeded(42));
        let clock = sim.signal("top.clock");
        sim.attach_clock(clock, Clock::with_period_ps(10_000));
        sim
    }

    /// The 512-stage chain qualifies and its static plan is the honest
    /// structure.  Region 0 is one eval settlement over every driver in
    /// topo order: `tx` first (its only input is the inert `r0`
    /// register, which propagates nothing, so `tx` carries no topo
    /// constraint), then the 512 register drivers in component order,
    /// then the four undriven `virt::Virt` interface outputs.  Region 1
    /// is the transfer point over all 512 registers in
    /// `clock_sensitivities` (declaration) order.  Region 2 is the
    /// combinational tail: `tx` (which reads `r0`'s live value) plus
    /// the 512 register drivers (dirty as transfer seeds, inert as
    /// propagators), while the interface outputs read nothing and stay
    /// out.
    #[test]
    fn chain_cycle_plan_regions() {
        let stages = 512;
        let mut sim = chain_sim(stages);
        let clock = sim.signal("top.clock");
        sim.attach_clock(clock, Clock::with_period_ps(10_000));
        let plan = sim.cycle_plan().expect("the chain must have a cycle plan");
        let vm::CyclePlanResult::Qualified(vm::cycle::CyclePlan { regions }) = plan else {
            panic!("the 512-stage chain must qualify, got {plan:?}");
        };
        let name = |id: SignalId| sim.full_name(id).to_string();
        assert_eq!(regions.len(), 3, "one eval, one transfer, one tail");

        let vm::cycle::Region::Eval { drivers, after_transfer: false } = &regions[0] else {
            panic!("region 0 must be the pre-transfer eval region");
        };
        assert_eq!(
            drivers.len(),
            stages + 5,
            "512 register drivers + tx + the 4 virt interface outputs"
        );
        assert_eq!(name(drivers[0].driver), "top.tx", "tx has no topo constraint");
        assert_eq!(
            drivers[0].inputs.iter().map(|s| name(*s)).collect::<Vec<_>>(),
            vec!["top.r0".to_string()],
            "tx's driver input list"
        );
        for (i, d) in drivers.iter().enumerate().skip(1).take(stages) {
            assert_eq!(name(d.driver), format!("top.r{}", i - 1), "register drivers in component order");
        }
        assert_eq!(
            drivers[stages + 1..]
                .iter()
                .map(|d| name(d.driver))
                .collect::<Vec<_>>(),
            vec!["top.exit", "top.spi_ck", "top.spi_cs", "top.spi_mosi"],
            "the undriven virt::Virt interface outputs close the topo order"
        );
        assert!(drivers[stages + 1].inputs.is_empty(), "interface outputs read nothing");

        let vm::cycle::Region::Transfer { registers, clock: plan_clock } = &regions[1] else {
            panic!("region 1 must be the transfer point");
        };
        assert_eq!(plan_clock, &clock, "the transfer's clock is the primary");
        assert_eq!(registers.len(), stages, "every register transfers");
        for (i, reg) in registers.iter().enumerate() {
            assert_eq!(name(*reg), format!("top.r{i}"), "registers transfer in declaration order");
        }

        let vm::cycle::Region::Eval { drivers, after_transfer: true } = &regions[2] else {
            panic!("region 2 must be the post-transfer tail");
        };
        assert_eq!(
            drivers.len(),
            stages + 1,
            "tx plus the 512 register drivers are downstream of the transfer"
        );
        assert_eq!(name(drivers[0].driver), "top.tx", "the tail keeps the topo order");
        for (i, d) in drivers.iter().enumerate().skip(1) {
            assert_eq!(name(d.driver), format!("top.r{}", i - 1), "the tail keeps the topo order");
        }
    }

    /// A register clocked by an internal wire (not the primary free-
    /// running clock) disqualifies the design, naming the register, the
    /// wire, and the primary clock.
    #[test]
    fn wire_clocked_register_disqualifies() {
        let mut sim = design_sim(
            "wire-clock",
            r"
pub mod Top {
    incoming clock : Clock
    outgoing out : Bit

    wire gated : Clock {
        it := clock
    }

    reg r : Word[8] on gated {
        it <= it + 1w8
    }

    out := r[0]
}
",
        );
        let clock = sim.signal("top.clock");
        let gated = sim.signal("top.gated");
        let reg = sim.signal("top.r");
        let plan = sim.cycle_plan().expect("the wire-clock design must have a verdict");
        let vm::CyclePlanResult::Disqualified(vm::Disqualification::RegisterClockedByNonPrimary { register, clock: bad_clock, primary }) = plan else {
            panic!("the wire-clocked design must disqualify, got {plan:?}");
        };
        assert_eq!(register, &reg, "the diagnostic names the register");
        assert_eq!(bad_clock, &gated, "the diagnostic names the wire");
        assert_eq!(primary, &clock, "the diagnostic names the primary clock");
    }

    /// A combinational cycle (a wire driver reading its own live value)
    /// is refused by the stage-A topological schedule, and the whole-
    /// design qualification falls back to that existing refusal.
    #[test]
    fn cyclic_design_falls_back_to_existing_refusal() {
        let sim = design_sim(
            "cyclic",
            r"
pub mod Top {
    incoming clock : Clock
    outgoing out : Bit

    wire a : Bit {
        it := it
    }

    reg r : Word[8] {
        it <= it + 1w8
    }

    out := r[0]
}
",
        );
        let plan = sim.cycle_plan().expect("the cyclic design must have a verdict");
        let vm::CyclePlanResult::Disqualified(vm::Disqualification::CyclicCombinationalGraph) = plan else {
            panic!("the cyclic design must fall back, got {plan:?}");
        };
    }

    /// The change-epoch skip, witnessed by the profiler counters (runs
    /// under `--features sim-profile`): in one rising-edge settlement
    /// of the 32-stage chain, every register's driver is visited twice
    /// (once for its own transfer's pop, once for its predecessor's),
    /// but the second visit's trigger epoch predates the driver's
    /// first evaluation this settlement, so exactly the repeat visits
    /// are skipped: 33 driver evals (32 register drivers + tx), 31
    /// skips.  The chain is 32 stages on purpose: the settlement must
    /// enter `flow` above `EPOCH_CHECK_MIN_DIRTY` for the check to run
    /// at all (small settlements keep the plain loop).  The falling
    /// edge visits nothing, and the next rising edge re-evaluates
    /// everything (the skip never carries across settlements), while
    /// the register values advance exactly as the chain defines.
    #[test]
    #[cfg(feature = "sim-profile")]
    fn epoch_skip_dedups_repeat_driver_visits_within_a_settlement() {
        let mut sim = chain_sim(32);
        assert!(sim.has_topo_schedule());
        let clock = sim.signal("top.clock");
        let clock_typ = sim.component_type_id(clock);
        let regs: Vec<SignalId> =
            (0..32).map(|i| sim.signal(format!("top.r{i}").as_bytes())).collect();

        let before: Vec<u64> = regs.iter().map(|&r| sim.get(r).to_u64()).collect();

        let settle = |sim: &mut Sim, high: bool| {
            let mut lock = sim.lock();
            lock.set(clock, Value::from_bool(clock_typ, high));
        };

        // Warm-up settlement (drives the clock low; may be a no-op
        // write), then measure the rising edge alone.
        settle(&mut sim, false);
        crate::sim::profile::reset();
        settle(&mut sim, true);
        let snap = crate::sim::profile::snapshot();
        assert_eq!(snap.evals_total, 33, "32 register drivers + tx, each once");
        assert_eq!(snap.epoch_skips, 31, "each driver's repeat visit is skipped");
        // Word[32] arithmetic wraps at 32 bits.
        const M32: u64 = 0xFFFF_FFFF;
        assert_eq!(sim.get(regs[0]).to_u64(), (before[0] + 1) & M32, "r0 advanced by one");
        assert_eq!(
            sim.get(regs[1]).to_u64(),
            (before[1] + before[0] + 1) & M32,
            "r1 = r1 + r0 + 1 over the pre-edge values"
        );
        assert_eq!(
            sim.get(regs[3]).to_u64(),
            (before[3] + before[2] + 1) & M32,
            "r3 = r3 + r2 + 1 over the pre-edge values"
        );

        // Falling edge: the clock's write has no AST reader, so no
        // driver is visited at all.
        crate::sim::profile::reset();
        settle(&mut sim, false);
        let snap = crate::sim::profile::snapshot();
        assert_eq!(snap.evals_total, 0, "no driver reads the clock by AST reference");
        assert_eq!(snap.epoch_skips, 0);

        // Second rising edge: the transfer re-dirties every register,
        // so every driver is re-evaluated - the skip is strictly a
        // within-settlement dedup and never carries across settlements.
        crate::sim::profile::reset();
        settle(&mut sim, true);
        let snap = crate::sim::profile::snapshot();
        assert_eq!(snap.evals_total, 33, "the next settlement re-evaluates every driver");
        assert_eq!(snap.epoch_skips, 31);
        assert_eq!(sim.get(regs[0]).to_u64(), (before[0] + 2) & M32, "r0 advanced again");
    }
}
