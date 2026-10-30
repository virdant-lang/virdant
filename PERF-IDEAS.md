# Ideas for further simulator speedups

This file collects candidate speedups for the Virdant simulator beyond
the VM redesign specified in `DESIGN.md`. The ranking evidence is
measured, not estimated:

- The fixed per-cycle floor is about 175 ns after the VM redesign. It
  bounds small designs (the one-stage bench runs at 5.4-6.4 MHz).
- The marginal cost per driver is about 5 ns per stage per cycle at the
  512-stage bench point. State bookkeeping around each driver dominates
  it, not the driver's computation.

Every idea should be judged by item 1 before and after implementation.
The differential harness in `sim-bench/` gates all of them.

## Zero: measure before cutting

1. **In-process ablation harness.** Kernel profilers are blocked in the
   common sandbox (`perf_event_paranoid=4`, `ptrace_scope=1`). A bench
   that drives `Sim` directly can time each phase separately (scheduler
   pop, flow loop, per-driver eval, commit, watcher sweep) behind a
   feature-gated counter. Cheapest item on the list, and it de-risks all
   the others.

## One: quick wins outside the engine

2. **Thin out `virt.rs` per-cycle work.** `FPS_CYCLES.store` is an
   atomic store per cycle; move accounting to once per thousand. The
   exit-port `sim.get` plus compare runs every cycle although the value
   stays `@Invalid()` nearly always; a raw-bits compare would do.
3. **Skip the watcher sweep when nothing changed.** `dispatch_pass`
   iterates the whole watcher table per settlement even when nothing
   changed. One settlement-level "changed" bit skips the sweep. The virt
   board registers about a dozen watchers.
4. **Batch VCD formatting.** Traces ride watchers; formatting per change
   can be buffered into larger writes if traced runs ever matter to
   throughput.

## Two: shrink the floor

5. **Native clock fast path.** One clock cycle is two scheduler events,
   each a heap push (with a fresh boxed callback allocation) plus a heap
   pop plus callback dispatch. A free-running clock with no user
   callbacks could be stepped directly in the run loop: toggle,
   transfer, flow, repeat. The general scheduler stays for everything
   else. Contained, and it attacks the biggest fixed cost.
6. **Falling-edge no-op detection.** Most designs clock registers only
   on rising edges, so the falling edge settles nothing but still pays
   full event machinery. If the dirty set contains only signals with no
   combinational fanout, skip straight to the sweep. Precompute "has
   combinational fanout" once.
7. **Clock-only settlements.** Generalization of item 6: if a rising
   edge's only effect is transferring registers, several bookkeeping
   steps collapse. Needs a semantics-identity argument; the differential
   harness makes it testable.
8. **Flatten sensitivity lists.** Store `Circuit`'s per-signal fanout
   lists in one arena with `(start, len)` slices, like `CodeRange`.
   Removes a pointer chase per dirty signal.
9. **Batch the dirty-list protocol.** `insert` does a bit test plus a
   stack push per changed signal; a `changed_bits |= incoming` merge
   could cut that, at the cost of exact LIFO order. Only viable if
   observation-order equivalence is proven, which is the same subtlety
   as item 17.

## Three: shrink the per-driver cost

10. **Bits-only commit path.** `Ret` constructs a full `Value`, then
    `set_val` compares `Value`s. For narrow signals the executor could
    hand back raw `u64` bits and let the commit compare integers,
    constructing a `Value` only when a read or the public API asks.
    Deletes two tag-carrying structs per fanout edge.
11. **Structure-of-arrays state.** Split `State::vals` into a bits array
    plus a side table of type tags. Narrow signals never touch `Value`
    inside the engine; `Sim::get` reconstitutes one at the API boundary.
    The arena rejected word-plane state on API grounds, but doing it
    internally with one construction point avoids that objection.
    Medium-size refactor, probably the biggest honest win for large
    designs within the current execution model.
12. **Unchecked frame slots.** `get_unchecked` on slots whose indices
    `Program::validate` already proved in range. Small, slightly unsafe,
    measurable; gate behind the validator.
13. **Superinstructions.** Fuse common emitted patterns (`Load` into
    `Bin`, `Extract` into `TestTag` chains) into single opcodes. Only
    worth it if the profiler shows dispatch is visible.
14. **Threaded code.** Compile each driver to a dispatch loop with
    inlined jump targets, removing range-lookup indirection. Modest, and
    the program gets less inspectable.

## Four: reduce how often drivers run

15. **Topological fanout ordering.** Sort each signal's fanout list so
    producers run before consumers within a settlement. Candidate C1
    called this semantics-neutral; the synthesis deferred it because
    evaluation order moves where a mid-propagation clock rising is
    observed. The differential harness can arbitrate now: if the golden
    suite holds, the risk was smaller than feared.
16. **Per-driver input-epoch caching.** A driver whose inputs have not
    changed since its last evaluation can be skipped even when it
    appears on a fanout it also reads. Value-numbering across
    settlements, keyed by an input-epoch stamp. Cheap per driver, and it
    composes with everything else.

## Five: the big structural bets

17. **The scheduled kernel, done principled.** Candidate C2's flaw was
    its commit-point constraint breaking on `r <= r + 1`. A resolution
    exists: topologically order the acyclic combinational region only,
    keep registers and clock wires on the event-driven path, and run
    cyclic cones (SCC islands) as bounded fixpoints where they occur.
    Commit points sit at the clock's topological position, reproducing
    mid-propagation transfer for acyclic readers. This is the direction
    that removes the per-signal bookkeeping the 512-stage measurement
    isolated, and it is where a 5-10x on large designs most plausibly
    lives. It is also the only item here that reopens semantics risk, so
    it deserves its own design pass.
18. **Bitplane execution.** Pack 64 one-bit signals into a `u64` and
    evaluate 64 per instruction, Verilator-style. Enormous for
    gate-level netlists, meaningless for word-oriented designs. Only if
    Virdant grows gate-level workloads.
19. **Native code generation.** The ISA was shaped so a backend could
    consume the same `Program`. Cranelift, or emitting Rust source and
    compiling, removes dispatch entirely. Costs: a dependency or a
    compile step, and slower design iteration.

## Six: parallelism and environments

20. **Off-thread VCD encoding.** Tracing serializes into the sim thread;
    a channel to a writer thread keeps the engine loop tight during
    traced runs.
21. **Multi-clock pipelining.** Independent clock domains could settle
    concurrently with a merge at shared signals. The merge logic is the
    hard part.
22. **Fix the profiling environment upstream.** `sysctl
    kernel.perf_event_paranoid=1` on the host would let `perf record`
    turn items 5-16 from hypotheses into a ranked list.

## Suggested sequence

Items 1, 2, 3, and 5 first: the profiler, the two caller-side
thinnings, and the native clock path. Small, independently verifiable
against the existing gates, and they attack the floor where the hello
number lives. Then items 10 and 11, the bits-only commit path and
structure-of-arrays state, which attack the 5 ns. Then decide between
item 17 and incremental shavings based on what the profiler says.
