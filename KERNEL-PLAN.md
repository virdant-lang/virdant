# Scheduled Kernel Plan

Status: stage B in flight (worker `db985adf`, launched from user go/no-go "just do the kernel schedule work").

---

## Part I — The problem

**Hardware is concurrent; the CPU is not.** A Virdant design is a net of
drivers, each a small program watching a set of input signals and producing
an output. In one clock cycle, hundreds or thousands of these drivers can
all "fire at once" — but a single-threaded CPU has to pick an order and fake
the concurrency.

The classic way to fake it is the **event-driven algorithm**, and that is
what the engine uses today: keep a worklist of signals whose values just
changed; pop one, find every driver watching it, evaluate those drivers, and
if a driver's output changes, push it on the worklist. Repeat until a full
pass over the worklist changes nothing — that is one *settlement*, and the
design is consistent again.

Event-driven is the standard because it is **O(activity)**: a design where
2% of the logic changes does 2% of the work. It is what VHDL and Verilog
simulators do, and it is why stage A's epoch-skip win was real (it made the
engine skip provably no-op re-visits, halving driver evaluations on the
dense chain).

The problem is what each unit of real work *costs around itself*. The
profiler decomposition, measured:

| Component of a cycle | s=1 bench | Dense chain | Dhrystone (real target) |
|---|---|---|---|
| Real evaluation work (driver evals) | ~180 ns | ~12 µs | ~2620 ns (90 evals × ~29 ns) |
| Real commit work (writes, register transfers) | ~500 ns | ~5.9 µs | proportional |
| **Machinery** (worklist pops, visit gathering, flow-loop entry/exit, dispatch sweep) | **~165 ns of a ~280 ns cycle** | ~2.5 µs | ~20 ns/settle × 7.5 settles |

On small designs the machinery is **more than half the cycle**. On the real
design it is small per settlement but paid 7.5 times per cycle. Three null
results this run proved this machinery is already at the event-driven floor
— you cannot shave it further *within the event-driven model*. The model
itself is the cost.

---

## Part II — The general technique: scheduling

The key observation that scheduling exploits: **the design's structure is
fixed at compile time; only the values change.** A CPU's driver graph is the
same graph on cycle 1 and cycle 1,000,000. So the *order in which drivers
must be evaluated* is not dynamic information — it is a property of the
netlist that can be computed once.

This idea is old and battle-tested. Verilator compiles Verilog into C++
where an entire cycle is a straight-line function: no worklist, no events,
just a fixed sequence of blocks in topological order. Cycle-based simulators
(for synchronous designs) precompute the full order and execute it every
cycle. Code-generating compilers (Chisel/FIRRTL → C++/Verilog) do the same.
The payoff is always the same: you delete the bookkeeping and pay only for
real work.

The spectrum, from least to most aggressive:

1. **Interpretive event-driven** (today's engine): maximum flexibility,
   maximum per-event overhead.
2. **Schedule caching** (stage B): observe which dirty-sets actually occur
   at runtime, and compile an execution order *for each recurring one*.
   The first occurrence is interpreted; every repeat runs straight-line.
3. **Static whole-cycle schedule** (stage C): compile the entire netlist
   into one program that computes a full clock cycle. No worklist at all.
4. **Full code generation**: emit machine code / C for the design.
   Verilator's endgame. Furthest possible step; not on this run's map.

---

## Part III — How it applies here

### Where we are

What already exists (banked prerequisites, not throwaway):

- Per-driver VM bytecode compilation (the whole VM rewrite, `DESIGN.md`
  ~line 277).
- `virdant/src/sim/vm/schedule.rs`: `TopoSchedule` — per-driver deduped
  input lists + topological order (Kahn); register drivers inert; cyclic
  graphs refuse → dynamic worklist fallback.
- Per-signal change-epochs + per-driver last-eval snapshots (stage A,
  commit `dce68b5`), trigger-epoch skip of repeat visits.
- Optimizer passes (folding, CSE, slot fusion, DCE) that run over bytecode.
- Profiler counters: `epoch_skips`, `topo_designs`/`topo_fallbacks`.

What does not exist yet: the kernel itself. The runtime flow loop is still
the dynamic worklist (pop dirty → gather visits → push/settle). Residual
machinery: ~165 ns of the s=1 bench's ~280 ns cycle floor; a chunk of the
dense chain's ~19 µs/cycle settlement.

### Stage A — schedule data (DONE, commit `dce68b5`)
Compile-time per-driver input lists + topo order + cycle detection,
validated and unit-tested. Trigger-epoch skip in the unchanged dynamic
loop. Measured: −20% on the 512-stage chain, evals halved (30.7M →
15.4M/100k cycles); even on s=1; noise-level on riscv/hello.

### Stage B — cached straight-line schedule (IN FLIGHT, worker `db985adf`)
At settlement entry, hash the dirty-set (signature). Cache hit → execute
the *stored, precompiled visit order* for exactly that dirty-set: no pops,
no gathering, no loop. Miss → run the dynamic loop once, then insert.
Invalidate on structural change (watcher registration, driver set changes,
`finish()` pruning). Bounded cache size with measured cap.

Why a cache rather than one global schedule: in a settlement, *which*
signals are dirty varies (a falling edge dirties only the clock; a register
write dirties a subset). A single static order would over-visit quiet
drivers, which is exactly what lost for the batch-topo variant during stage
A (+14% on the chain). Caching per dirty-set keeps the O(activity) property
while making repeats free. In a synchronous design the same small set of
dirty-set signatures recurs essentially every cycle, so the cache fills in
the first few cycles and then everything is a hit.

- Soundness contract: the cached order must reproduce the dynamic loop's
  exact visit sequence per dirty-set (not merely any valid topo order), or
  watcher firing order and trace bytes shift. The dynamic loop's visit
  sequence is deterministic (stack pop order × sensitivity-list order), so
  for a given dirty-set there is exactly one legal sequence. The signature
  must be wide enough that two dirty-sets with different visit orders can
  never collide on one key. Proof in comments + tests, not assumed.
- Untouched pinned contracts: watcher dispatch order, `ClockHandle` seq
  allocation, `changed_since_dispatch`, nested `unlock()`, `finish()`,
  `transfer_clock`, RNG draw order.
- Gates: `sim-bench/check.sh` ×2 (byte-identical traces), `cargo test
  -p virdant`, `cargo test --workspace`, `make test`.
- Method: phase-1 simprof decomposition before design; A/B paired,
  interleaved, ≥3 rounds/bench, both binaries from the SAME cargo session
  (unit M lesson); dhrystone FPS not an A/B instrument (unit L lesson);
  benches: `build/s1`, `build/s512perf`, `assets/new`, `~/projects/virdant-riscv`.
- Exit: if the cache cannot beat the dynamic loop on BOTH scaling points,
  commit nothing, report the null with the decomposition. No regressions
  ship.
- Estimate: 3-5 h (null exit possible in 1.5-2 h; hard-soundness case
  longer).

### Stage C — whole-cycle straight-line compilation (THE ENDGAME, not started)
Compile the entire netlist into one straight-line program: each driver's
bytecode becomes an inlined chunk; the chunks are ordered topologically;
one call computes a full clock cycle. What disappears entirely: the
worklist, the pops, the gather, the flow loop, the per-settlement ceremony.
What remains: real evaluation and commit work, plus stage A's epoch-skip
machinery as *activation predicates* inside the program (so sparse designs
stay O(activity)).

- Expected gain: 2-5x engine-level (100-400%); stacks multiplicatively with
  stages A/B.
- This is a multi-session project with its own gates and its own run. It is
  gated on stage B's outcome being green and on the operator's review of
  its numbers. Not auto-launched.

### Risks, honestly

| Risk | Mitigation |
|---|---|
| Cached order diverges from dynamic order → wrong trace bytes | Signature width proof + 7-trace byte-identity gates ×2, run before any perf claim |
| Cache blows up on chaotic designs (many distinct dirty-sets) | Bounded size with measured cap; eviction policy; miss-fallback is always correct |
| Stage C forces O(design) per cycle on sparse designs | Stage A's epoch-skip machinery becomes the schedule's activation predicate; sparse bench is on the checklist to *measure* this, not assert it |
| Glitch semantics | The dynamic loop's order *defines* the deterministic glitch resolution; both stages replay that exact order, so semantics are identical by construction |
| All this work nulls out | Discipline: null is an acceptable deliverable; nothing ships that regresses |

### The payoff, in one table

| Design | Today | After stage B (est.) | After stage C (est.) |
|---|---|---|---|
| s=1 bench | ~280 ns/cycle (~165 machinery) | ~130-180 ns/cycle | ~80-120 ns/cycle |
| 512-stage chain | ~19 µs/cycle | ~16-18 µs/cycle | ~10-14 µs/cycle |
| Dhrystone (real design) | ~1555 ns/cycle real work | ~1400-1500 ns/cycle | ~700-1200 ns/cycle |
| End-to-end wall | ~0.26 MHz | ~0.28-0.30 MHz | ~0.5-0.8 MHz |

The ranges are wide because they inherit the honest uncertainty of this
run's measurement discipline (±17% on dhrystone end-to-end, ±5%
build-to-build) — every number above needs its own paired measurement
before it becomes a claim.

---

## Part IV — Interim levers (if stage B nulls or before stage C)

| Lever | Est. gain on dhrystone | Confidence |
|---|---|---|
| Builtin-callback coalescing (direct injection + one settle/tick) | 30-60% (0.7 → 0.9-1.1 MHz) if half the callback flow is spurious; floor ~5-10% if all genuine | Medium |
| Alias-chain collapse (compile-time pass rewiring readers to sources) | 15-30% | Medium-high |
| Cache-layout work on state storage | 15-30% | Low, speculative |
| Bits-only commit path | landed (unit E, biggest paired win) | — |
| Scheduled kernel (stages B+C) | 100-400% engine-level | High on mechanism, big effort |

---

## Part V — Discipline (non-negotiable, from the run's lessons)

1. Measure-first: simprof decomposition before any design is committed.
2. A/B pairs from one cargo session only; ±5% build-to-build LLVM spread
   makes cross-session pairs invalid (unit M).
3. Dhrystone end-to-end FPS swings ±17% on identical binaries — never an
   A/B instrument (unit L).
4. Differential byte-identity ×2 before any perf claim (trace bytes).
5. Null results are deliverables; regressions are not.
6. Worker lands → coordinator verifies independently (gates ×2, full
   suites, paired simprof) → only then does the commit move to
   `sim-vm-rewrite` HEAD.

---

## Evidence trail

- Audit TSV: `.audit/vm-sim-rewrite.tsv`
- Cross-model review: `.audit/trail-cross-review.md`
- Transcript audit: `.audit/transcript-audit.md`
- Scope decisions recorded in `PERF-IDEAS.md` items 20/21 (no threading,
  no push/PR — deliverable is local commits on `sim-vm-rewrite` only).
