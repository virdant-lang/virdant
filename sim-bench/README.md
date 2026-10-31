# `sim-bench` — simulator benchmarks and the differential-trace harness

This directory holds the simulator benchmark tooling and the
differential-trace harness used to prove simulator-engine equivalence.

## Generating a bench design

`gen.py` writes a parameterizable, never-exiting Virdant project:
`--stages N` chains N `Word[32]` registers (xor + add) driven by one
clock, so per-cycle work scales linearly with the stage count.

```sh
python3 sim-bench/gen.py --stages 8 --out build/simtrace-bench/stages-8
```

The design is written as `DIR/src/top.vir` plus a `DIR/virdant.toml`
with `top = "top::Top"`.  Generated designs are never committed.

## Tracing a design: `virdant/examples/simtrace.rs`

`simtrace` builds the `Db` for a project, constructs a `Sim` with
`InitStrategy::Seeded(S)`, runs N clock cycles, and writes one line per
observed (cycle, signal) pair:

```sh
cargo run --release -p virdant --example simtrace -- \
    --project build/simtrace-bench/stages-8 --cycles 16 --seed 42 \
    --out /tmp/stages-8.trace
```

Flags: `--project DIR` (required), `--cycles N` (default 16), `--seed S`
(default 0), `--out FILE` (default stdout), `--top PATH` (default
`top::Top`).

Output format:

```
# virdant-trace v1
# top=top::Top seed=42 cycles=16 signals=1034
# format: <cycle> <path> <hex> <bin>
0 top.clock 0 0
0 top.r0 c9980276 11001001100110000000001001110110
...
```

Every elaborated component is dumped each cycle (via `Sim::components`,
the exhaustive flat component list), sorted by path.  `<hex>` is the
value zero-padded on the left to whole nibbles; `<bin>` is the full
MSB-first bit string, so the pair is unambiguous for any width.  The
initial, fully-settled state is cycle 0; each later snapshot is taken
inside an `on_clock` rising-edge watcher on the attached clock, i.e.
after the edge has transferred every register and the combinational
network has re-settled (see the header comment in `simtrace.rs`).

Two runs with the same `--seed` over the same design produce
byte-identical output.

## Golden capture and check

```sh
sim-bench/capture.sh    # recapture sim-bench/golden/<case>.trace
sim-bench/check.sh      # recapture + diff against golden; PASS/FAIL per case
```

The case list lives in both scripts: generated bench designs at stages
1, 8, 32, 128, 512, the `assets/new` hello project, and the
`tests/sim/gcd` design (all at seed 42, 16 cycles).  The generated
designs go under `build/` and are not committed; the golden traces
under `sim-bench/golden/` are.

`.gitignore` ignores `*.sh` globally, so the scripts are force-added
(`git add -f sim-bench/*.sh`).

## What the check proves

Today: `check.sh` proves the **current** engine is deterministic — two
consecutive runs must both PASS — and freezes its per-cycle behavior in
the committed golden traces for the listed designs.

After the simulator rewrite (the planned bytecode VM): the golden
traces stay frozen; `check.sh` runs the *new* engine through the same
runner and diffs against them.  Any FAIL is a behavioral difference
between the new engine and the current one on the captured designs and
seed.  Passing means the rewrite is observably equivalent on these
traces: identical initial seeded state, identical register-transfer and
combinational-settlement results, cycle by cycle, for every signal.

What it does *not* prove: equivalence on designs outside the case list,
on other seeds, on timing/callback semantics not observable at the
per-cycle dump point, or on performance.  Extend the case list (or add
stimulus-driving cases) as coverage needs grow.

## Phase attribution: `virdant/examples/simprof.rs`

Kernel profilers (`perf`, gdb) are blocked in the common sandbox, so
phase attribution comes from the `sim-profile` cargo feature: counters
in the engine itself (`virdant/src/sim/profile.rs`), compiled out to
zero cost when the feature is off.  `simprof` drives a design the same
way `simtrace` does (seeded init, free-running clock on `top.clock`,
N cycles) and prints the phase table:

```sh
cargo build --release -p virdant --example simprof --features sim-profile
target/release/examples/simprof --project build/simprof-bench/stages-1 \
    --cycles 100000 --seed 42
```

Flags: `--project DIR` (required), `--cycles N` (default 100000),
`--seed S` (default 0).

Columns (absolute time and per-cycle nanoseconds):

- `run total`: the whole `Sim::run` call.
- `scheduler`: time in `Scheduler::next_after_delay` (heap pop plus
  time advance), per timed event.  Clock edges on the free-running
  fast path do not enter the scheduler, so on a clock-dominated run
  this column now attributes only user (`at`/`after`) events.
- `sched push (est)`: time in `Scheduler::register`'s timed-heap push
  (user-event registrations; clock edges on the free-running fast
  path do not push), stride-256 sampled.
- `flow`: time in `Sim::flow` settlements (exact: one `Instant` pair
  per settlement).  Includes the driver evals and the in-flow
  `set_val` commits.
- `eval (est)`: time in `eval_driver`, stride-256 sampled.  A
  breakdown of `flow`, not an addition to it.
- `commit (est)`: time in `State::set_val` (all callers) plus the
  rising-edge register-transfer loops in `Sim::transfer_clock`
  (stride-256 and stride-8 sampled respectively).  The in-flow
  `set_val` part overlaps `flow`.
- `sweep`: time in `Sim::dispatch_pass` (watcher-table sweeps), exact.
- `floor`: run total minus scheduler, push, sweep, flow, and measured
  commit work: the per-cycle cost not attributed to any measured
  scale-dependent phase (event dispatch, callback bookkeeping,
  lock/transfer glue, the run loop itself).

The counts line is exact everywhere: events (timed `AfterDelay`
dispatches; clock edges on the free-running fast path are counted
separately as `clock_edges`), pushes, dirty pops,
driver evals (all on the compiled VM), change-epoch skips (`epoch_skips`:
driver visits within a settlement whose triggering signal was not
re-written since the driver's last evaluation this settlement - a
repeat visit is a provably no-op re-commit; see
`virdant/src/sim/vm/schedule.rs`), set_val/transfers calls and
actual writes, rising edges, watcher passes and fires.  The `build:`
line reports how many designs compiled with a static combinational
schedule (`topo_designs`) or fell back to the pure dynamic worklist
because their driver graph has a cycle (`topo_fallbacks`).

Rules for reading the numbers:

- Numbers are only meaningful phase-vs-phase within one run.  Each
  timed or sampled entry carries roughly two `Instant` reads (the
  printed `read_cal` is the calibrated single-read interval); with the
  feature on the whole run is slower than without it, and the `floor`
  row inherits the instrument's own per-entry cost (the notes line
  prints the timed entries per cycle).  Never compare absolute times
  across feature settings or across instruments.
- Sampled rows are estimates: per-call = `max(0, mean sample interval
  - read_cal)`, scaled by the exact call count.  They are exact for
  homogeneous calls and approximate otherwise; a category with very
  few samples is noise.
- To rank an optimization, run simprof before and after with the same
  design, seed, and cycle count, and compare phase-vs-phase.
