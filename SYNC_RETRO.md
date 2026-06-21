# Retrospective: Implementing SYNC_PLAN.md

This document is a retrospective on the implementation of clock domain
tracking in Virdant, as specified in SYNC_PLAN.md.
It covers what was built, what went wrong, what went well,
and lessons for future work.

## Scope of Changes

142 files changed, +1772 / -808 lines.
The work spans the entire compiler pipeline:
lexer, parser, AST, type system, analysis, simulation, and Verilog
generation.
Every `.vir` file in the repository was touched to add explicit clock
domain annotations.

### New source files

- `virdant/src/types/typing/domain.rs` (645 lines)
  The domain inference and checking pass.
  This is the core of the feature.

### New test files

- `tests/pass/sync_async_basic.vir`
  Demonstrates `sync()`, `async()`, and `on <clock>` annotations.
- `tests/pass/sync_debounce.vir`
  A complete debounce module using `sync()` for metastability protection.
- `tests/pass/multi_clock_crossing.vir`
  Safe CDC between two clock domains using `async()` then `sync()`.
- `tests/errors/clock_domain_crossing.vir`
  Tests E0707 for combining signals from different domains.
- `tests/errors/uninferrable_clock.vir`
  Tests E0708 for signals with no inferrable clock domain.

### Modified source files (16 files, +666 / -51 lines)

- `virdant/src/types/typ.rs`
  Added the `ClockDomain` enum with `Async`, `OnClock(SymbolId)`,
  and `Unknown` variants.
  Includes `Display`, equality, and helper methods.
- `virdant/src/types.rs`
  Re-export `ClockDomain`.
- `virdant/src/types/typing.rs`
  Added `mod domain` and the call to `check_domains` in `typecheck`.
- `virdant/src/types/typing/infer.rs`
  `ExprSync` and `ExprAsync` infer their type from their argument.
- `virdant/src/syntax/token.rs`
  Added `KwAsync` and `KwSync` keyword tokens.
- `virdant/src/syntax/payload.rs`
  Added `AsyncAnnotation`, `ExprSync`, `ExprAsync` AST payloads.
- `virdant/src/syntax/ast.rs`
  Added new payloads to `is_expr()` and `kind()`.
  Generalized `clock()` to work for all component kinds (not just
  `Reg`/`OutgoingReg`), skipping `AsyncAnnotation` markers.
  Added `async_annotation()` and `clock_domain_annotation()` accessors.
- `virdant/src/syntax/virdant.lalrpop`
  Replaced `OnClause` with `ClockDomain` production supporting both
  `async` and `on <expr>`.
  Added `sync(x)` and `async(x)` to `ExprAtom`.
- `virdant/src/syntax/tests.rs`
  Added 6 parser tests for the new syntax.
- `virdant/src/analysis/component.rs`
  Added `clock: Option<ClockDomain>` to `Component`.
  Added `clocks_in_scope` to `ComponentAnalysis`.
  Implemented `discover_clocks()` and `infer_component_clock()`
  with semantic validation.
- `virdant/src/analysis/ports.rs`
  Added `clock: Option<ClockDomain>` to `Port`.
  Updated `build_ports_of()` to populate it.
- `virdant/src/analysis/package.rs`
  Added expr roots for all components with `on` clauses.
  Suppressed `MissingOnClause` for async regs.
- `virdant/src/analysis/elaboration.rs`
  Only `Reg`/`OutgoingReg` are clocked in simulation.
  Wires with `on clock` stay combinational.
- `virdant/src/queries/typing.rs`
  All component kinds with `on` clauses get `Type::Clock` as expected
  type for the clock reference expression.
- `virdant/src/diagnostics.rs`
  Added 11 new diagnostics (E0701-E0708, W0751-W0753).
- `virdant/src/verilog/conversion.rs`
  `ExprAsync` passes through (no hardware).
  `ExprSync` passes through (TODO: double-flop).
  Wires with `on clock` and `:=` drivers stay as `wire` elements.
- `virdant/src/sim/expr.rs`
  `ExprSync`/`ExprAsync` treated as pass-through in simulation.

### Modified `.vir` files (120 files)

Every `.vir` file in `tests/`, `examples/`, `docs/`, and `riscv/` was
updated to add explicit `async` or `on <clock>` annotations to signals
that previously had none.
Format golden files were regenerated.

## What Went Wrong

### 1. The core checking pass was initially skipped

The most serious mistake.
Phases 1 (parsing), 2 (analysis), and 4 (Verilog) were implemented,
but Phase 3 (the actual domain checking) was only partially done.
The infrastructure was built (the `ClockDomain` enum, the `clock`
field on `Component`, the diagnostic definitions), but the constraint
collection and unification algorithm was never written.
The `DomainCrossing` (E0707) and `UninferrableClock` (E0708)
diagnostics were defined but never triggered.

This meant that code like the following compiled without error:

```virdant
mod Foo {
    incoming clk_a : Clock
    incoming clk_b : Clock
    incoming data_a : Bit on clk_a
    wire data_b : Bit on clk_b
    data_b := data_a  // Should be E0707!
}
```

The user caught this and rightfully called it out.
The whole point of the feature is to catch these errors.
This was fixed by implementing `virdant/src/types/typing/domain.rs`,
a 645-line module that does fixpoint domain inference and checking.

### 2. The E0708 check was missing even after E0707

After implementing E0707, a further gap remained:
signals with no annotation and no inferrable domain were silently
accepted.
The user pointed out that this should be an error:

```virdant
mod Foo {
    incoming inp : Bit       // No clock, no annotation
    outgoing out : Bit {     // No clock, no annotation
        it := inp
    }
}
```

This was fixed by adding the E0708 check at the end of
`check_domains()`, after inference completes.
Any component whose domain is still `Unknown` produces an error.

### 3. The migration script corrupted inline it-blocks

The first migration script used a regex that stopped at `{`,
which truncated inline it-blocks like `wire a : Bit { it := 0 }`
into `wire a : Bit async {`.
This broke several test files.
The script was rewritten to properly track bracket depth and
preserve inline blocks.

### 4. Wires with `on clock` were treated as registers

The Verilog converter and elaboration pass treated any component
with an `on clock` clause as a clocked register.
This is correct for `reg` but wrong for `wire`:
a wire with `on clock` is combinational (its domain is for type
checking only, not for sequential behavior).
This caused simulation panics and incorrect Verilog output.

Fixed by:
- In `elaboration.rs`: only `Reg`/`OutgoingReg` get a clock assigned.
- In `conversion.rs`: wires with `on clock` are declared as `wire`,
  not `reg`.
  Only wires with `<=` (latched) drivers are upgraded to `reg`.

### 5. Declaration order broke inference

The initial inference pass was single-pass:
it walked statements in source order and inferred domains.
But Virdant allows forward references:
a wire can be declared before the register it depends on.

For example, in the FIFO test:

```virdant
wire is_full : Bit {
    it := read_is_write && different_lap
}
// ... later ...
reg read : Word[3] on clock
```

Here `is_full` depends on `read_is_write`, which depends on `read`,
which is declared later.
A single pass cannot infer `is_full`'s domain.

Fixed by switching to fixpoint iteration:
keep running the inference pass until no new domains are inferred.
Diagnostics from intermediate passes are discarded;
only the final pass's diagnostics are kept.

### 6. Guard expressions were not checked

The `when` statement's guard expressions were not visited during
domain inference.
This meant that signals used only in guards (like `reset`) could not
be inferred.

Fixed by inferring guard domains using the enclosing component's
domain as the expected domain.
If the target is `Unknown` but the guard is `Known`, the target is
inferred from the guard.

## What Went Well

### 1. Grammar changes were clean

The LALRPOP grammar changes compiled without conflicts on the first
try.
The `ClockDomain` production naturally rejects `async on clk`
(mutual exclusivity) because after matching one alternative, the next
token does not fit `ItBlock?`.

### 2. The `ClockDomain` enum design was sound

The decision to use `SymbolId` (not a string) in `OnClock(SymbolId)`
meant that clock identity is structural:
two references to the same clock are equal by construction.
This avoids string comparison bugs and makes cross-module resolution
straightforward.

### 3. Fixpoint iteration handled complex cases

Once fixpoint iteration was in place, the inference pass correctly
handled the FIFO's forward references, the debounce's `reset` signal
(inferred from `when` guard context), and combinational wires in
clocked modules.
No manual annotation was needed for these cases.

### 4. The test suite caught regressions

The existing test suite (62 cargo tests + integration tests) was
invaluable for catching regressions.
Every time a fix was made, running `make test` revealed the next
issue.
The format tests, in particular, caught cases where the migration
script corrupted source files.

### 5. The diagnostic definitions were reusable

Defining all 11 diagnostics upfront (E0701-E0708, W0751-W0753) meant
that when the checking pass was finally implemented, the diagnostics
were already wired up and tested.
The `IsDiagnostic` trait pattern made adding new diagnostics
straightforward.

## Lessons Learned

### 1. Implement the checking pass first

The most important lesson.
A type system feature without enforcement is just syntax.
The checking pass is the deliverable;
everything else is infrastructure.
In future, the checking pass should be implemented and tested with
error cases before migrating existing code.

### 2. Test with the user's exact example

The user provided a concrete example of code that should error but
did not.
This was the most effective possible test case.
In future, when implementing a checking feature, start by writing the
error test case and making it pass.

### 3. Migration scripts need to respect syntax

The migration script corrupted inline it-blocks because it used a
naive regex.
For a language with nested syntax, a proper parser-based migration
tool would be safer.
At minimum, the script should track bracket depth.

### 4. Fixpoint iteration is worth the complexity

Single-pass inference is simpler but breaks on forward references.
Fixpoint iteration is slightly more complex but handles real-world
code patterns.
The added complexity is in the inference driver, not in the inference
logic itself.

### 5. Domain annotations should not change code generation

The bug where `wire ... on clock` became a `reg` in Verilog was a
violation of the design principle that domain annotations are for
type checking only.
They should not change the generated hardware.
This principle should be documented and enforced in the converter.

## Remaining Work

### Phase 4: Double-flop synchronizer generation

`sync(x)` currently passes through its argument directly in Verilog
generation.
The plan calls for a double-flop synchronizer (two registers in
series).
This is a TODO in `virdant/src/verilog/conversion.rs`.

### Phase 3: Full domain inference for all expression types

The domain checker handles the common cases (references, binary ops,
drivers, `when`/`match`), but some expression types may not be fully
covered:
- `ExprStruct` (struct construction)
- `ExprCtor` (constructor calls with complex arguments)
- Nested function calls

These default to `Unknown`, which may produce false E0708 errors in
edge cases.

### Cross-module domain checking

The plan calls for checking clock domain compatibility at module
boundaries (e.g., connecting an async signal to a clocked port).
This is not yet implemented.
The `Port` struct has the `clock` field, but the connection checking
logic is not written.

### Strict mode

The plan mentions a future "strict mode" requiring explicit
annotations on all signals.
This is deferred.
Currently, inference is the default and only mode.

## Conclusion

The implementation delivers the core value of clock domain tracking:
the compiler now catches clock domain crossing errors (E0707) and
uninferrable domain errors (E0708).
The feature is exercised by 5 new test files and the entire existing
test suite (migrated to use explicit annotations).

The biggest mistake was initially skipping the checking pass.
The biggest win was the fixpoint inference algorithm, which handles
real-world code patterns without requiring explicit annotations
everywhere.

## Architectural Aside: Fused vs. Separate Queries

Domain checking lives inside `types/typing.rs` as a second phase
within the `typecheck()` function.
But it is a completely independent AST walk with zero code sharing
with the type checker.

The type checker (`check.rs` / `infer.rs`) handles *type* inference
and checking: Bit, Word[n], struct, enum, etc.
It uses top-down checking with bottom-up inference, a single pass
per expression root, `TypingContext`, and `ExpectedType`.

The domain checker (`domain.rs`) handles *clock domain* inference
and checking: Async, OnClock(SymbolId), Unknown.
It uses fixpoint iteration with its own `inferred` map,
walks moddef-level statements (not expression roots),
and depends only on `ComponentAnalysis`.

They share no data structures, no inference logic, and no diagnostic
types.
The type checker does not know about `ClockDomain`.
The domain checker does not know about `Type`.
The only thing that fuses them is the call site in `typecheck()`.

### Why this is awkward

**Namespace confusion.**
`types/typing/` suggests the module is about the type system:
inferring whether an expression is `Bit`, `Word[8]`, or a struct.
But `domain.rs` lives there too, doing something with no relation
to types.
The name `Typing` is overloaded: it means both "what type is this?"
and "what clock domain is this?"

**Algorithmic divergence.**
The type checker is single-pass, top-down, using `TypingContext`.
The domain checker is fixpoint, walking statements, using an
`IndexMap` it manages itself.
They share no algorithmic DNA.

**No shared dependencies.**
The domain checker does not use `TypingContext`, `ExprRoot`,
or `ExpectedType`.
It only uses `ComponentAnalysis`, which is built during the analysis
phase (before type checking).
This means it is not really a sub-phase of type checking at all.
It is a validation/analysis pass that happens to run at that point
in the pipeline.

### What separation could look like

The domain checker could live in a separate query, for instance
`queries/domain.rs`, exposed as:

```rust
pub(crate) fn check_domains(
    builder: &mut Builder,
    symbol_id: SymbolId,
    moddef_node: &AstNode,
) -> Vec<Diagnostic>;
```

It would still be called from `typecheck()` in `typing.rs` the same
way it is now.
Only the module location and the namespace would change.

### When fusing would make sense

If domain checking and type checking actually *interacted*, they
would belong together.
For example, if the type of `sync(x)` depended on the clock domain
of `x` (e.g., a `Typed` struct combining both), or if the domain
checker used inferred types to make domain decisions.

Neither is true today.
`sync(x)` preserves the type of `x` regardless of domain.
`async(x)` preserves the type of `x` regardless of domain.
The domain checker never looks at the type checker's output.

### Recommendation

Move `domain.rs` out of `types/typing/` into its own query module,
likely `queries/domain.rs` (or `analysis/domain_check.rs`).
This clarifies that domain checking is a peer of type checking,
not a subordinate phase.
The name `queries/` fits because domain checking is a validation
query: given component analysis and an AST, produce diagnostics.

This is a pure code organization change with no behavioral impact.
It is worth doing if and when the module structure is being
decluttered.
