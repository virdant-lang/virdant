# `sim-bench` — simulator benchmarks and the differential-trace harness

This directory holds the simulator benchmark tooling and the
differential-trace harness used to prove simulator-engine equivalence.

The repository carries two simulation engines behind the `Sim` facade
(`virdant/src/sim/sim.rs`):

- the **VM engine** (`Engine::Vm`), the compiled-bytecode simulator —
  `vir run`'s default;
- the **legacy engine** (`Engine::Legacy`), the tree-walking
  interpreter — `vir run --legacy`.

The legacy engine is the **reference**: it is the engine the repo's
test-suite was written against.  The harness proves the VM engine
reproduces its behavior exactly.

## Generating a bench design

`gen.py` writes a parameterizable, never-exiting Virdant project:
`--stages N` chains N `Word[32]` registers (add) driven by one
clock, so per-cycle work scales linearly with the stage count.  The
optional `--width W` switches the register type to `Word[W]`: wide
modes exercise the wide-value paths (`--width 128` stays within the
inline BitPat representation, `--width 200` goes through the heap
one).

```sh
python3 sim-bench/gen.py --stages 8 --out build/simtrace-bench/stages-8
```

The design is written as `DIR/src/top.vir` plus a `DIR/virdant.toml`
with `top = "top::Top"`.  Generated designs are never committed.

## Tracing a design: `virdant/examples/simtrace.rs`

`simtrace` builds the `Db` for a project, constructs a `Sim` with
`InitStrategy::Seeded(S)` on a selected engine, runs N clock cycles,
and writes one line per observed (cycle, signal) pair:

```sh
cargo run --release -p virdant --example simtrace -- \
    --project build/simtrace-bench/stages-8 --cycles 16 --seed 42 \
    --engine legacy --out /tmp/stages-8.trace
```

Flags: `--project DIR` (required), `--cycles N` (default 16),
`--seed S` (default 0), `--out FILE` (default stdout), `--top PATH`
(default: auto-resolve the `::Top` module), `--engine vm|legacy`
(default `vm`).

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
The dump point, the seeded initial state, and the trace format are
engine-independent (the trace carries no engine field), so a VM run
and a legacy run over the same design and seed are directly diffable.
Designs with no clock on their top module get only the cycle-0
snapshot.

Two runs with the same `--seed` and `--engine` over the same design
produce byte-identical output.

## Golden capture and check

```sh
sim-bench/capture.sh    # recapture sim-bench/golden/<case>.trace (legacy engine)
sim-bench/check.sh      # recapture with BOTH engines; diff against golden and each other
sim-bench/sweep.sh      # differential sweep over EVERY tests/sim/* and tests/run/* design
```

The case list lives in both scripts: generated bench designs at stages
1, 8, 32, 128, 512, the wide bench (512 stages at Word[128], golden
`bench-stages-512-w128`), the `assets/new` hello project, and the
`tests/sim/gcd` design (all at seed 42, 16 cycles).  The generated
designs go under `build/` and are not committed; the golden traces
under `sim-bench/golden/` are.

`.gitignore` ignores `*.sh` globally, so the scripts are force-added
(`git add -f sim-bench/*.sh`).

## What the checks prove

`check.sh` proves, on its case list:

1. the **legacy** engine still reproduces the frozen golden traces
   (`--engine legacy` vs the committed golden), and
2. the **VM** engine is observably equivalent to the legacy engine on
   those designs and seed: identical initial seeded state, identical
   register-transfer and combinational-settlement results, cycle by
   cycle, for every signal.

`sweep.sh` extends (2) to every design under `tests/sim/*` and
`tests/run/*`.  Where both engines fail on a design, the failure
output must also be identical (modulo panic thread ids); such designs
are reported as `SKIP-both-fail` — they exercise constructs the
shared sim lowering does not support, which is reference (legacy)
behavior, not an engine divergence.

What these checks do not prove: equivalence on designs outside the
swept set, on other seeds, on timing/callback semantics not
observable at the per-cycle dump point, or on performance.  Extend
the case list (or add stimulus-driving cases) as coverage needs grow.
