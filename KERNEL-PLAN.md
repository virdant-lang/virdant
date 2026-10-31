# Scheduled Kernel Plan

Status: stage B in flight (worker `db985adf`, launched from user go/no-go "just do the kernel schedule work").

## Where we are

The scheduled kernel is the endgame identified in `PERF-IDEAS.md` item 17:
compile each clock cycle into straight-line code so the runtime pays no dirty
list, no settlements, no dispatch sweep. Three micro-optimization units landed
before it (stage A, bits-only commit, watcher buckets); three measured nulls
(falling-edge gate, sweep skip, settlement fixed cost) proved the dynamic
engine is at its floor. The kernel is the only remaining big lever.

What already exists (banked prerequisites, not throwaway):

- Per-driver VM bytecode compilation (the whole VM rewrite, `DESIGN.md` ~line 277).
- `virdant/src/sim/vm/schedule.rs`: `TopoSchedule` — per-driver deduped input
  lists + topological order (Kahn); register drivers inert; cyclic graphs
  refuse → dynamic worklist fallback.
- Per-signal change-epochs + per-driver last-eval snapshots (stage A,
  commit `dce68b5`), trigger-epoch skip of repeat visits.
- Optimizer passes (folding, CSE, slot fusion, DCE) that run over bytecode.
- Profiler counters: `epoch_skips`, `topo_designs`/`topo_fallbacks`.

What does not exist yet: the kernel itself. The runtime flow loop is still
the dynamic worklist (pop dirty → gather visits → push/settle). Residual
machinery: ~165 ns of the s=1 bench's ~280 ns cycle floor; a chunk of the
dense chain's ~19 µs/cycle settlement.

## Stages

### Stage A — schedule data (DONE, commit `dce68b5`)
Compile-time per-driver input lists + topo order + cycle detection, validated
and unit-tested. Trigger-epoch skip in the unchanged dynamic loop. Measured:
−20% on the 512-stage chain, evals halved (30.7M → 15.4M/100k cycles); even
on s=1; noise-level on riscv/hello.

### Stage B — cached straight-line schedule (IN FLIGHT, worker `db985adf`)
At settlement entry, hash the dirty-set (signature). Cache hit → execute the
precompiled driver order for that exact dirty-set: no pops, no gathering, no
dynamic loop. Miss → run the dynamic loop, then insert. Invalidate on
structural change (watcher registration, driver set changes, `finish()`
pruning). Bounded cache size with measured cap.

- Soundness contract: the cached order must reproduce the dynamic loop's
  exact visit sequence per dirty-set (not merely any valid topo order), or
  watcher firing order and trace bytes shift. Proof in comments + tests.
- The signature must not collide across dirty-sets with different visit
  orders; widen the key until impossible.
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
  commit nothing, report the null with the decomposition. No regressions ship.
- Estimate: 3-5 h (null exit possible in 1.5-2 h; hard-soundness case longer).

### Stage C — whole-cycle straight-line compilation (THE ENDGAME, not started)
Compile the entire netlist into one program that computes a whole clock cycle
in one pass. No settlements, no dispatch, no flow at runtime. Per-driver
bytecode becomes inlined chunks. Foundation: stage A's topo order, stage B's
schedule shapes, the optimizer passes.

- Expected gain: 2-5x engine-level (100-400%); stacks with stages A/B.
- This is a multi-session project with its own gates and its own run. It is
  gated on stage B's outcome being green and on the operator's review of its
  numbers. Not auto-launched.

## Interim levers (if stage B nulls or before stage C)

| Lever | Est. gain on dhrystone | Confidence |
|---|---|---|
| Builtin-callback coalescing (direct injection + one settle/tick) | 30-60% (0.7 → 0.9-1.1 MHz) if half the callback flow is spurious; floor ~5-10% if all genuine | Medium |
| Alias-chain collapse (compile-time pass rewiring readers to sources) | 15-30% | Medium-high |
| Cache-layout work on state storage | 15-30% | Low, speculative |
| Bits-only commit path | landed (unit E, biggest paired win) | — |
| Scheduled kernel (stages B+C) | 100-400% engine-level | High on mechanism, big effort |

## Discipline (non-negotiable, from the run's lessons)

1. Measure-first: simprof decomposition before any design is committed.
2. A/B pairs from one cargo session only; ±5% build-to-build LLVM spread
   makes cross-session pairs invalid (unit M).
3. Dhrystone end-to-end FPS swings ±17% on identical binaries — never an
   A/B instrument (unit L).
4. Differential byte-identity ×2 before any perf claim (trace bytes).
5. Null results are deliverables; regressions are not.
6. Worker lands → coordinator verifies independently (gates ×2, full suites,
   paired simprof) → only then does the commit move to `sim-vm-rewrite` HEAD.

## Evidence trail

- Audit TSV: `.audit/vm-sim-rewrite.tsv`
- Cross-model review: `.audit/trail-cross-review.md`
- Transcript audit: `.audit/transcript-audit.md`
- Scope decisions recorded in `PERF-IDEAS.md` items 20/21 (no threading,
  no push/PR — deliverable is local commits on `sim-vm-rewrite` only).
