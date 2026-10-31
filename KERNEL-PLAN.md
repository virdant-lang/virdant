# Scheduled Kernel Plan

Status: **the scheduled kernel is CLOSED at stage C** — the endgame unit
(whole-cycle compiled program) fired the null exit clause: after stage C
the chain's remaining capturable machinery is ~2.5-5.5% of the cycle
(below the 10% bar), and the one design with large machinery (riscv) is
structurally unqualifiable (registers clocked by combinational internal
signals — a static whole-cycle program would change committed register
values). No commit; tree clean at `262bf53`. The decomposition also
corrected this plan's own 1.4x estimate: it double-counted the
rising-edge transfer loop (~5.3k, real commit work) and sampler bias as
machinery — true machinery is ~0.3-0.7k, and eval+commit ~9.8-10.3k of a
~12.85k cycle is irreducible real work. See Part VI for the full
decomposition.

Stage C (`fa4147e` + `f599211`), stage B (`791047e`), stage A
(`dce68b5`) beneath it.

## Stage C measured results (paired A/B, interleaved pairs, one cargo session)

Mechanism: the recorder watches every eligible settlement's dirty-set
bitmap; when the last P signatures exactly repeat the P before them
(hash pre-filter, exact verification), the pattern promotes to the
replay schedule — a sequence of phases, each (dirty-set bitmap,
topological driver list).  An active settlement checks its phase's
bitmap (full-width, element-wise — exact, not hashed) and executes the
phase's drivers once each with no worklist, no signature hashing, no
per-settlement cache lookup, and no epoch machinery.  Mismatch falls
through to the unchanged stage-B/dynamic path; structural changes
(watcher registration, `finish()`) reset everything.  Soundness is per
settlement and structural (see `vm::schedule::CycleSchedule`): for a
terminal settlement the committed outcome is a pure function of the
entry dirty set, so any settlement whose set matches a phase replays
correctly whatever produced the set.

| bench | stage B (234acb9) | stage C | verdict |
|---|---|---|---|
| s=1 | ~274 ns/cyc | ~280 ns/cyc | pre-fix: **+1.7% consistent** (replay bookkeeping on trivial 1-2-dirty settlements). After floor fix `f599211`: **+1.0% consistent residual** (273.5→276.3 median, 15 ABBA pairs) — better than pre-fix but not zero; inside the ±2% bar. Mechanism: the per-settlement prologue (state check + dirty-length compare) stage B's capture path did not have. Worker's "s1 flat" claim was optimistic; recorded honestly. |
| s=512 chain | ~15300 ns/cyc | ~13290 ns/cyc | **−13.1% vs stage B after the floor fix** (coordinator ABBA ×9: B 13120-13703 vs A 14755-15443); win fully retained — the floor's giveback on the 1-dirty falling edge is nil (it is cheaper dynamic than replayed), and warm-up is faster (period-1 detection once the falling edge is excluded) |
| hello | ~412 ns/cyc | ~413 ns/cyc | flat (coordinator ABBA ×4); never engages (phase build refuses its settlements; recorder off after 4 failures) |
| riscv | ~1348-1397 ns/cyc | ~1360-1398 ns/cyc | flat (coordinator ABBA ×4, inside noise); recorder gives up after 1024 captures, then one state load per settlement |

Counters on the chain: 2 replays/cycle, 0 aborts, 1 activation,
epoch_skips 51.1M → 1022 (the replay never attempts the provably-no-op
repeat visits), driver_evals unchanged at 51.3M (the same real
evaluations — the soundness prediction), dirty_pops 100513 → 516.

Worker-process note: an early `std::env::var` debug probe left in
`flow` cost ~45 ns/cycle on hello and survived every intermediate
measurement; it is gone from the shipped tree, and hello's flat pairs
above are from the clean build.

## Floor fix (`f599211`) — what it does and what it cost

`CycleSchedule::MIN_ENGAGEMENT = 3` (schedule.rs:606). Settlements whose
entry dirty-set length is below 3 are excluded from recording **and**
from replay in every state: no capture entry, no match attempt, no
phase advance, no miss — fully transparent to the pattern cursor, so
the recorded sequence is just the sub-sequence of above-floor
settlements and the period-1 rising pattern on the chain detects
sooner. Below-floor settlements still feed the give-up guard in
Recording (all-tiny designs still reach recorder-off) but not the
thrash guard in Active (the chain's 1-dirty falling edge must not
drain tolerance). Two soundness tests added; gates green on the
merged tree (check.sh ×2, 102 virdant tests, workspace clean, make
test).

Worker deviations, accepted after review: (1) the stage-C instrument
timer pairs were removed from the committed tree — they fired only in
stage C binaries, so they had biased the s1 comparison by the
instrument's own per-pair cost; (2) `cycle_captured.take()` tails are
gated on the same armed-state check as the prologue (provably no-ops
when disarmed).

Coordinator verification (merged tree `fa4147e`): re-ran gates ×2 (ALL
PASS), 100 virdant tests, workspace 120, `make test` green; re-measured
all four benches with own interleaved pairs — s512 **−13.0%** (15.6k→13.6k,
5/5, tight), hello/riscv flat, s1 **+1.7% consistent** (worker's "flat"
was inside its ±2-3% band but 5/5 directional; phase split: flow +4.0,
floor +4.3 instrument, sweep −0.8, i.e. ~+2 ns/replay of bookkeeping on
trivial settlements).  Also confirmed the worker's disclosure items: the
wired cache-lookup timers and the removed debug probe.

Stage B's riscv +1.8% codegen cost is untouched: stage C adds no code
to the dynamic loop (its machinery is out-of-line helpers gated behind
one state load), so the regression is neither recovered nor worsened
by this stage.

## Stage B measured results (coordinator, interleaved pairs)

| bench | stage A (dce68b5) | stage B (791047e) | verdict |
|---|---|---|---|
| s=1 | 280.9 ns/cyc | 279.8 ns/cyc | flat (cache eligible but marginal) |
| s=512 chain | 18,097 ns/cyc | 15,598 ns/cyc | **−13.8%, all 3 pairs** |
| hello | 417.5 ns/cyc | 417.4 ns/cyc | flat (thrash guard disables cache; misses=0 recorded) |
| riscv | 1364 ns/cyc | 1388 ns/cyc | +1.8%, 8/8 pairs; cache never engages (misses=0), identical work counts — hot-loop codegen cost, inside the ±2% bar; recovery candidate if it persists |

Gates on the merged tree: `sim-bench/check.sh` ×2 ALL PASS, `cargo test
-p virdant` 93 passed, workspace 113 passed, `make test` green.

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

### Stage B — cached straight-line schedule (DONE, commit `791047e`;
coordinator-verified: chain −13.8%, s1/hello flat, riscv +1.8% inside
the bar — see measured results above). The text below is the original
brief, kept for the record.
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

### Stage C — deep-cycle replay (LANDED as `fa4147e` + floor fix
`f599211`, coordinator-verified — see measured results above). NOTE: the
shipped shape is NOT the whole-cycle compilation originally sketched
below. What actually shipped: a recorder watches settlement dirty-set
signatures; when the last P exactly repeat, the pattern promotes to a
replay schedule of precompiled phases (dirty bitmap + topo driver list)
executed with no worklist/hash/epoch machinery, with a thrash guard,
bounds pre-check, and engagement floor. The original endgame sketch is
kept below for the record; the remaining unshipped idea (inlining all
driver bytecode into one cycle-level program) is still the ceiling if
another big step is ever wanted.
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

---

## Part VI — Endgame unit: null result and the honest ledger (CLOSED)

The whole-cycle compiled-program unit (the Verilator-style endgame) was
launched with an explicit exit clause: if the phase-1 decomposition
showed the remaining machinery below ~10% of the cycle on every bench,
report the null and stop. It fired.

Measured decomposition after stage C (temporary per-component timers,
instrument self-cost ~280 ns/cyc corrected by interval arithmetic):

| component (512-stage chain) | ns/cycle | share | verdict |
|---|---|---|---|
| driver evaluation (513 VM evals) | ~4400-4850 | ~35% | irreducible real work |
| commit: clock toggle + 512 register transfers | ~5280-5470 | ~41% | irreducible real work (512 transfers × ~10.3 ns) |
| rising-replay ceremony (prologue, match, advance, clear) | ~90-400 | ~1-3% | capturable remainder |
| falling-edge settlement (1 dirty, dynamic) | ~28 | <0.5% | capturable |
| watcher sweep (2 passes, updates last even when not firing) | ~40-56 | ~0.4% | mostly irreducible |
| step_clock glue + run-loop glue | ~200-250 | ~2% | capturable |

**Total capturable ≈ 300-700 ns ≈ 2.5-5.5%** — below the 10% bar and at
the edge of the ±2% build-lottery noise band.

Key correction to this plan's own estimate: the "~3.6k machinery → 1.4x
ceiling" figure double-counted the rising-edge register-transfer loop
(~5.3k, real commit work) and sampled-estimator bias as machinery.
Measured eval+commit is ~9.8-10.3k of a ~12.85k true cycle; machinery is
~0.3-0.7k, not 3.6k. The 1.4x endgame did not exist after stage C; the
honest headroom is ~3-6%.

riscv: structurally unqualifiable. The design runs 21 register transfers
per cycle across ~14 mid-settlement rising events (registers clocked by
combinationally driven internal signals). A mid-settlement transfer
commits the pending value at the moment of the rise; reproducing that
requires the exact dynamic visit/transfer sequence — which is precisely
the stage C recorder mechanism, and it already gave up there (1024
captures, non-repeating 1→15 growth). A static whole-cycle program would
change committed register values on such designs; correctness-by-
construction forbids it. hello: non-terminal combinational tails, never
engages any schedule — same disqualification. s1: machinery ~40-60 ns,
absolute prize ~50 ns, not a target.

Qualification condition the compile would have needed (for the record):
single primary free-running clock; every register clocked only by a
primary input signal (the soundness-critical clause that excludes
riscv); acyclic TopoSchedule; runtime guards (empty dirty set at edge
entry, no pending user event before the cycle's last edge, no structural
change). The chain qualifies — and on the chain the prize measures
~3-6%.

No commit; all instrumentation reverted; tree clean at `262bf53`; gates
re-verified green (check.sh ×2, virdant 102, workspace 122, make test).

**Follow-up lever named by the decomposition (out of scope for the
scheduled kernel):** inside the VM interpreter — chain eval ~8.8
ns/eval sampled for `it + prev + 1w32`, dispatch/frame overhead of ~10
bytecode ops. Fused dispatch / superinstructions is the remaining
engine-level lever, and it is a different unit from the scheduled
kernel.
