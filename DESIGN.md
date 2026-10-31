# Virdant simulator VM design

This document specifies the redesign of the Virdant simulator
(`virdant/src/sim/`) as an optimizing bytecode VM. It records the problem
evidence, the chosen design, the semantics the VM must preserve, the
verification gates, and the migration sequence.

## Tutorial: reading VM assembly

This tutorial teaches you to read the VM's assembly. By the end you will
read a compiled Virdant driver and know exactly what the simulator does
with it. You need only basic RISC-V vocabulary: registers, immediates,
and branches.

### The machine model

The VM is closer to RISC-V than to a stack machine. Start with four
differences.

- There is no stack and no calls. A program is one straight line of
  instructions with a few internal branches.
- Instead of 32 registers, each program owns a frame of numbered slots:
  `s0`, `s1`, and so on. The compiler decides the count at compile time.
- Slots hold raw bits, not typed values. Width is a compile-time fact.
  Operations mask on write, like an `andi` with the mask baked in.
- The one output is `ret sK`. It hands slot `K`'s bits to the simulator,
  which tags them with the signal's type. Every program has exactly one
  `ret`, and control flow must converge on it.

Reading design state is the one memory-like operation. `load s, sig`
copies a signal's current value into a slot, like `lw` from a fixed
address.

| RISC-V | This VM |
|---|---|
| register `t0` | slot `s0` |
| `li t0, 1` | `const s0, 1` |
| `lw t0, ADDR` | `load s0, sig` |
| `add t2, t0, t1` | `bin.add s2, s0, s1` |
| `bnez t0, L` | `brtruthy s0, L` |
| return value in `a0` | `ret s0` |

### Program 1: a counter

Here is a Virdant register. On each clock edge it either resets to 0 or
increments.

```virdant
reg count : Word[8] on clock {
    when {
        case reset {
            it <= 0w8
        }
        else {
            it <= it + 1w8
        }
    }
}
```

The compiler emits this program:

```
count:
    load      s0, count        ; the "it" reference: own live value
    load      s1, reset
    brtruthy  s1, Lreset       ; cond is a Bit: test bit 0
    const     s2, 1            ; literal 1, 8 bits
    bin.add   s3, s0, s2       ; width-8 add, masked to 8 bits on write
    jmp       Ldone
Lreset:
    const     s3, 0
Ldone:
    ret       s3               ; attaches TypeId(Word[8]): next value
```

Trace it. Say `count` is 41 and `reset` is 0.

1. `load s0, count` puts 41 in `s0`.
2. `load s1, reset` puts 0 in `s1`.
3. `brtruthy` tests bit 0 of `s1`. It is 0, so we fall through.
4. `const` and `bin.add` compute 42 into `s3`.
5. `jmp` skips the reset arm. `ret s3` hands back 42.

Now say `reset` is 1. Step 3 takes the branch instead. `s3` becomes 0,
and `ret s3` hands back 0. Same program, both behaviors, no
interpreter in between.

One subtlety: `ret` does not change `count` immediately. The value
becomes the register's next value, and the simulator commits it when
`clock` rises. That commit rule lives outside the program, in `flow`.

### Program 2: match on an enum

Virdant enums are packed words. The tag lives in the low bits and
payloads sit above it, much like a struct-of-bitfields in C. `State`
has three constructors, so the tag is 2 bits, and two `Word[8]`
payloads bring the width to 18 bits.

```virdant
wire next : State {
    it := match state : State {
        case @Idle()        => @Running(x, y)
        case @Running(a, b) => @Done(a)
        else                => @Idle()
    }
}
```

The compiler emits this program:

```
next:
    load      s0, state             ; the subject, raw bits
    load      s1, x                 ; eager inputs of later arms
    load      s2, y
    testtag   s0, 0, Lidle          ; (bits >> 0) & 3 == 0
    testtag   s0, 1, Lrunning
    jmp       Lother                ; @Done and anything else

Lidle:
    encode    s5, 0, []             ; @Idle(): tag bits only
    jmp       Lout
Lrunning:
    extract   s3, s0, 2, 8          ; payload a = bits[10..2]
    extract   s4, s0, 10, 8         ; payload b = bits[18..10]
    encode    s5, 2, [(s3, 2)]      ; @Done(a): tag 2, payload at bit 2
    jmp       Lout
Lother:
    encode    s5, 0, []
Lout:
    ret       s5                    ; attaches TypeId(State)
```

`testtag` is the enum version of a RISC-V compare chain. It computes
`(bits >> lo) & mask`, compares against the arm's tag, and branches on
equality. In RISC-V you would write `srli`, `andi`, `beq` for each arm.
Here it is one instruction with `lo`, the mask, and the tag baked in as
immediates.

`extract` is a shift and a mask. It pulls one payload out of the
subject. `encode` goes the other way. It builds the output word from a
tag and payload slots at fixed bit offsets. Both offset sets come from
the type's layout, computed once at compile time.

Trace one case. Say `state` holds `@Running(3, 9)`. The tag bits are 1,
`a` is 3 at bits 2 to 9, and `b` is 9 at bits 10 to 17.

1. The first `testtag` fails. The second matches, so we land on
   `Lrunning`.
2. `extract s3, s0, 2, 8` puts 3 in `s3`.
3. `encode s5, 2, [(s3, 2)]` builds tag 2 with 3 spliced at bit 2.
4. `ret s5` hands back the bits of `@Done(3)`.

If the match had no `else` clause, the compiler would emit a hold
epilogue instead of `Lother`: a slot preloaded with `next`'s own live
value, and `ret` from that slot. An unmatched subject then keeps the
old value. That is the `Owner` hold rule from the semantics section.

### What is deliberately missing

Compared with RISC-V, this machine has no stack, no calls, no memory
addressing, no flags, and no untyped values. Branch targets are fixed
at compile time. Slots are untyped and width-classed, so a wide value
cannot appear in a narrow operation. A program allocates
nothing when it runs: every slot exists before the first instruction,
which is the property that makes the simulator fast.

### Check yourself

1. Why must both arms of Program 1 converge on one `ret`?
2. `state` holds tag 2 with 7 in the payload of `@Done`. Which
   instructions run, and what does `ret` hand back?
3. In Program 2, why does `Lidle` and `Lother` emit the same `encode`?

Answers.

1. A program has exactly one `ret`. The compiler routes every arm to a
   common epilogue slot, so the terminator is unique by construction.
2. The two loads run, then both `testtag`s fail, so we fall through to
   `Lother`, which encodes `@Idle()` (tag 0). `ret` hands back tag 0
   with no payload. Note `@Done` is not read here, because the `else`
   arm ignores it.
3. `@Idle()` and the `else` arm both produce `@Idle()`. Identical
   instructions are a sign the compiler folded nothing here; the
   peephole pass may later merge the two paths.

## Why rewrite the simulator

The simulator this rewrite replaced interpreted driver expressions
directly. Each driven component carried an `Expr` tree (a 28-variant
interpreter AST in `sim/expr.rs`). On every fanout edge of every changed
signal, `Sim::flow` walked one tree and evaluated it under a freshly
built `Context`.

Measured on a release build (`vir run --fps-no-cap`, commit `60a0880`,
before the rewrite):

- A one-stage generated register chain sustains about 4.5 MHz. An
  eight-stage chain sustains the same rate. The cost is a fixed per-cycle
  floor of roughly 222 ns, not design size.
- The same chain at 512 stages sustains 0.33 MHz. The marginal cost is
  about 5 ns per stage per cycle.
- The `assets/new` hello design reports 0.15 MHz. Its UART program is
  cycle-hungry, so the floor dominates it.

The walkthrough of the current engine attributes the floor to these
per-cycle costs:

- One fresh `Context` (`IndexMap<Referent, Value>`) allocation per
  fanout-edge evaluation, plus one `Value` clone per binding.
- `BitPat` materialization for `BinOp` operands wider than the inline
  path, and again per match arm through `decode_ctor`.
- `Context::extend` clones the whole parent map per matched arm.
- An `IndexSet<SignalId>` dirty worklist: hash inserts, LIFO pops.
- A linear min-scan plus `Vec::remove` in `Scheduler::next_after_delay`,
  run twice per clock cycle, because one cycle is two half-period
  scheduler events.
- A full watcher-table sweep per settlement.

Everything structural (sensitivity lists, resolved referents, type
layouts) is already precomputed in `Circuit`. The interpretation layer is
the target.

## Goals and non-goals

Goals:

- Replace the interpretation layer with a custom bytecode and an
  optimizing VM designed for Virdant designs.
- Keep the steady-state path allocation-free for values of 64 bits and
  narrower.
- Give optimizing passes a stable format to work on, so passes never
  require a format change.
- Preserve the simulator's observable behavior exactly.

Non-goals:

- No JIT or native code generation. The measured bottleneck is
  allocation churn, not dispatch.
- No change to the public API or to simulation semantics. In particular,
  no Verilog-style NBA phase is introduced.
- No change to `vir run`, the `virt` board, builtins, or VCD output.

## What stays the same

The event-driven settlement model is the execution core and does not
change:

- `Sim::flow` pops a dirty worklist and re-evaluates the fanout of each
  changed signal until the fixpoint. Event-driven sparsity is kept.
- Registers transfer mid-propagation. A clock wire that rises during
  `flow` transfers its registers immediately, inside the same
  settlement, and the transferred values continue to propagate. There is
  no separate commit phase.
- Socket aliases (`:=:`) are runtime copies taken in `flow`, including
  edge detection on the copy. A clock routed through a socket still
  transfers.
- `initial` statements run once, then `flow_all` drives the full network
  to a fixpoint. Random initialization makes change detection useless at
  t=0, so construction uses the full fixpoint, not the dirty worklist.
- Watchers (`on_change`, `on_clock`) fire in registration order, swept to
  quiescence, only in the outermost `unlock`. Nested unlocks run `flow`
  only. The comment at `sim.rs` around line 355 documents why. It guards
  a fixed past bug.
- The public surface stays frozen: `Sim`, `SimLock`, `Clock`, `Ctor`,
  `Value`, `TypeId`, `BitPat`, `InitStrategy`, and `builtins::attach`
  keep their signatures. `vir/src/vir/virt.rs` and
  `virdant/examples/simgcd.rs` must compile unchanged.

One additive exception: `InitStrategy::Seeded(u64)` joins `Random` so the
differential harness can reproduce runs bit-for-bit.

## The instruction set

The unit of compilation is one driver: the expression that computes one
component's value. Each driver compiles to a straight-line instruction
sequence with internal branches for `when` and `match`, ending in one
`Ret`.

Two address spaces exist because value widths are compile-time facts.
Narrow slots hold the raw bits of a value of 64 bits or fewer in one
`u64`. Wide slots hold a `BitPat` for wider values. Every
value-producing operation is monomorphized by the compiler into the
variant whose operand spaces match the static widths. No runtime
operation dispatches on width class.

```rust
pub(super) type Ip = u32;
pub(super) type SlotN = u16;   // narrow slot index
pub(super) type SlotW = u16;   // wide slot index

#[derive(Clone, Copy, Debug)]
pub(super) enum Dst {
    Narrow(SlotN),
    Wide(SlotW),
}

#[derive(Clone, Copy, Debug)]
pub(super) enum Truthiness { Bit, NonZero }

#[derive(Clone, Debug)]
pub(super) enum Ins {
    // Boundaries: the only instructions that touch State.
    Load { signal: SignalId, dst: Dst },
    LoadUnresolved { dst: Dst },

    // Constants. Slots hold bits only; Ret attaches the type tag.
    Const { bits: u64, dst: SlotN },
    WideConst { bits: BitPat, dst: SlotW },
    Zero { dst: Dst },

    // Control flow (when / match).
    Jmp { to: Ip },
    BrTruthy { cond: SlotN, kind: Truthiness, to: Ip },
    TestTag { subj: SlotN, tag_lo: u16, tag_mask: u64, tag: u64, next_if_match: Ip },
    TestTagWide { subj: SlotW, tag: BitPat, next_if_match: Ip },
    Extract { subj: SlotN, lo: u16, width: u16, dst: SlotN },
    ExtractWide { subj: SlotW, lo: u16, width: u16, dst: Dst },
    TestBits { subj: SlotN, value: u64, next_if_match: Ip },
    TestBit { subj: SlotN, bit: bool, next_if_match: Ip },

    // Algebra, monomorphized on operand space.
    Bin { op: BinOp, width: u16, lhs: SlotN, rhs: SlotN, dst: SlotN },
    BinWide { op: BinOp, lhs: SlotW, rhs: SlotW, dst: Dst },
    Un { op: UnOp, width: u16, subj: SlotN, dst: SlotN },
    UnWide { op: UnOp, subj: SlotW, dst: Dst },

    // Construction from pre-decoded layout geometry.
    EncodeCtor { total_width: u16, tag: u64, tag_lo: u16,
                 parts: Box<[(SlotN, u16)]>, dst: SlotN },
    EncodeCtorWide { typ: TypeId, symbol: SymbolId,
                     args: Box<[SlotW]>, dst: SlotW },
    Splice { total_width: u16, parts: Box<[(SlotN, u16)]>, dst: SlotN },
    SpliceWide { parts: Box<[(SlotW, u16)]>, dst: SlotW },

    // Indexing and width ops.
    Index { subj: SlotN, index: u16, dst: SlotN },
    IndexDyn { subj: SlotN, index: SlotN, dst: SlotN },
    IndexWide { subj: SlotW, index: u16, dst: SlotN },
    IndexDynWide { subj: SlotW, index: SlotN, dst: SlotN },
    IndexRange { subj: SlotN, lo: u16, hi: u16, dst: SlotN },
    IndexRangeWide { subj: SlotW, lo: u16, hi: u16, dst: Dst },
    Zext { subj: SlotN, dst: SlotN },
    Sext { subj: SlotN, dst: SlotN },
    ZextWide { subj: SlotW, dst: Dst },
    SextWide { subj: SlotW, dst: Dst },
    Move { src: SlotN, dst: SlotN },     // as / cast / as Clock: bits pass through
    MoveWide { src: SlotW, dst: SlotW },
    Any { subj: SlotN, dst: SlotN },
    All { subj: SlotN, dst: SlotN },
    AnyWide { subj: SlotW, dst: SlotN },
    AllWide { subj: SlotW, dst: SlotN },

    // Unsupported payloads panic lazily (`todo!()`), as the old
    // interpreter did.
    Trap,

    // Terminator.
    Ret { src: SlotN },
    RetWide { src: SlotW },
}
```

Design notes:

- No type tags live in the frame. Today every `Value` carries an interned
  `TypeId` and every operator re-tags through `Expr::typ_id`. In the VM a
  slot holds bits only. The single tag attachment happens at `Ret`, which
  builds one `Value` from the driver's compile-time type and width.
- Width classes live in the instruction variants, not in flags. A wide
  value cannot flow into a narrow operation because no such instruction
  exists.
- `Load` and `Ret` are the only instructions that know `State` and
  `Value` exist. Everything between them is pure bit arithmetic and is
  testable without a simulator.
- `Trap` covers `Method`, `Fn`, and `StrLit` payloads, which were
  `todo!()` in the interpreter. The panic stays lazy: a design
  whose never-taken branch contains a `Method` still simulates, exactly
  as it always has.

## The instruction set by example

The ISA is the Rust enum above. This section shows the same instructions
in a friendly assembly spelling, one mnemonic per variant, to make the
shape easy to read. The spelling is illustrative. The real format is the
`Ins` enum.

Each mnemonic maps to one variant:

| Mnemonic | Variant |
|---|---|
| `load s, sig` | `Load` |
| `const s, bits` | `Const` (or `WideConst`) |
| `bin.op dst, a, b` | `Bin` (or `BinWide`) |
| `brtruthy c, L` | `BrTruthy` |
| `jmp L` | `Jmp` |
| `testtag s, tag, L` | `TestTag` |
| `extract dst, s, lo, w` | `Extract` |
| `encode dst, tag, parts` | `EncodeCtor` (or `EncodeCtorWide`) |
| `ret s` | `Ret` (or `RetWide`) |

Slot names `s0`, `s1`, ... are narrow frame slots. Every operation
computes on raw bits. The compiler knows each slot's width, so operations
mask on write and `ret` attaches the type tag.

### Example 1: a counter register

Virdant source (a register driver; `it` is the register's own value):

```virdant
reg count : Word[8] on clock {
    when {
        case reset {
            it <= 0w8
        }
        else {
            it <= it + 1w8
        }
    }
}
```

Compiled bytecode:

```
count:
    load      s0, count        ; the "it" reference: own live value
    load      s1, reset
    brtruthy  s1, Lreset       ; cond is a Bit: test bit 0
    const     s2, 1            ; literal 1, 8-bit
    bin.add   s3, s0, s2       ; width-8 add, masked to 8 bits on write
    jmp       Ldone
Lreset:
    const     s3, 0
Ldone:
    ret       s3               ; attaches TypeId(Word[8]): next value
```

Both arms converge on one `Ret`, which is the only terminator a driver
has. A register driver's `ret` produces the next value; `flow` writes it
through `set_reg` and transfers it when the clock rises.

### Example 2: match with payload extraction

Virdant source (an enum with three constructors; the tag lives in the
low 2 bits, payloads above):

```virdant
wire next : State {
    it := match state : State {
        case @Idle()        => @Running(x, y)
        case @Running(a, b) => @Done(a)
        else                => @Idle()
    }
}
```

Compiled bytecode:

```
next:
    load      s0, state             ; the subject, raw bits
    load      s1, x                 ; eager inputs of later arms
    load      s2, y
    testtag   s0, 0, Lidle          ; (bits >> 0) & 3 == 0
    testtag   s0, 1, Lrunning
    jmp       Lother                ; @Done and anything else

Lidle:
    encode    s5, 0, []             ; @Idle(): tag bits only
    jmp       Lout
Lrunning:
    extract   s3, s0, 2, 8          ; payload a = bits[10..2]
    extract   s4, s0, 10, 8         ; payload b = bits[18..10]
    encode    s5, 2, [(s3, 2)]      ; @Done(a): tag 2, payload at bit 2
    jmp       Lout
Lother:
    encode    s5, 0, []
Lout:
    ret       s5                    ; attaches TypeId(State)
```

Compare this to the interpreter it replaces. `eval_match` decoded the
subject once per arm through `decode_ctor`, materializing a heap `BitPat`
each time, and `Context::extend` cloned the whole binding map per matched
arm. Here one `load` brings the subject in, each arm costs one shift,
one mask, and one compare, each bound variable is one `extract`, and
construction splices pre-computed offsets. No allocation happens
anywhere on this path.

The `else` arm here encodes `@Idle()` directly. When a match has no else
clause, the compiler emits a hold epilogue instead: a slot preloaded with
the driver's own live value, and `ret sH` from it. That is the
`Referent::Owner` hold semantics, visible as two instructions.

## The program and frames

```rust
pub(super) struct CodeRange { start: Ip, len: u32 }

pub(super) struct Program {
    code: Box<[Ins]>,                      // all drivers, concatenated
    entry: Vec<Option<CodeRange>>,         // per SignalId; None = no driver expression
    frames: Vec<FrameLayout>,              // per SignalId
    driver_typs: Vec<TypeId>,              // the tag Ret attaches
}

pub(super) struct FrameLayout { n_narrow: u16, n_wide: u16 }
```

A `Frame` is one component's scratch space: a `Vec<u64>` for narrow slots
and a `Vec<BitPat>` for wide slots. The `Vm` allocates one frame per
driven component once, at `Sim::new`, and reuses it for every evaluation.
The steady-state hot path never allocates a frame.

Slot indices are valid only inside their own component's frame. The
executor indexes `frames[comp.index()]`, so nothing can express a
cross-component slot reference.

Every driven component lowers to bytecode. The temporary interpreter
fallback is gone: there is no unlowered construct. Conditions that would
once have routed a driver back to the interpreter (operand width
equality, range bounds, ctor geometry) are type-checker invariants, and
the compiler panics at compile time if one is violated. The interpreter
(`sim/eval.rs`) is deleted; `Value` lives in `sim/value.rs`.

## The executor

`Vm::run` is a flat loop over `Program::code`, one `match` arm per
variant. Every arm advances `ip` by one except the five jump and test
forms and `Ret`.

```rust
impl Vm {
    pub(super) fn run(&mut self, program: &Program, state: &State,
                      comp: SignalId) -> Value {
        // not implemented sketch
        // let range = program.entry[comp.index()].expect("no driver");
        // let frame = &mut self.frames[comp.index()];
        // match &program.code[ip] {
        //     Ins::Bin { op, width, lhs, rhs, dst } => {
        //         frame.narrow[*dst as usize] =
        //             bin_narrow(*op, *width, frame.narrow[*lhs as usize],
        //                        frame.narrow[*rhs as usize]);
        //     }
        //     Ins::Ret { src } => {
        //         return Value::from_u64(typ, width, frame.narrow[*src as usize]);
        //     }
        //     ...
        // }
    }
}
```

Programs are validated once, after compilation: every range ends in a
`Ret`, every jump target is inside its own range, and every slot index is
within its `FrameLayout`. The checks run as debug asserts. The executor
trusts the program afterward.

The dominant access pattern per settlement is: `DirtyList::pop`, then for
each fanout edge either an alias copy or one `Vm::run`. Inside a run,
each executed reference is one indexed `Vec` read, each operation is one
masked `u64` computation, and the terminator builds one `Value`. Nothing
on the narrow path allocates.

## Compilation

All stages run once inside `Sim::new`. Nothing retains `Db` after
construction.

```
Db + top
  |
  v
[1] circuit.rs (existing, reused)
    elaboration, type interning, layouts, dependencies, sensitivities,
    clock sensitivities, aliases, registers, initial expressions,
    resolved referents
  |
  v
[2] expr.rs (existing, reused)
    driver_to_expr: AST driver -> Expr IR
    Expr becomes a compile-time intermediate. It is no longer stored
    for runtime use
  |
  v
[3] vm/compile.rs (new), per driven component
    a. slot assignment: resolved referents -> frame slots, DFS order
    b. constant folding and branch propagation (on the IR)
    c. layout strength reduction: decode_ctor/encode_ctor geometry
       becomes TestTag/Extract/EncodeCtor/Splice immediates
    d. emit Ins; Ret terminator
  |
  v
[3'] peephole pass (new), per driver
    dead-instruction elimination, slot fusion, CSE
  |
  v
[4] link
    concatenate code ranges; fill entry, FrameLayout, driver_typs;
    Program::validate (debug)
```

Optimizer passes, and where each runs:

1. Constant folding and branch propagation, on the IR before codegen.
   It deletes whole `when` and `match` subtrees and collapses `initial`
   drivers, which are constant by construction.
2. Layout strength reduction, at lowering time. It replaces runtime
   `decode_ctor` and `encode_ctor` calls with static geometry. This is
   the largest single win for match-heavy designs.
3. Peephole dead-code elimination, slot fusion, and CSE, on emitted
   bytecode. Passes rewrite only instruction sequences inside one
   `CodeRange`.
4. Topological work ordering is admitted by the format but deliberately
   not built. Reordering evaluation changes where a mid-propagation
   clock rising is observed, which is semantic risk with no measured
   need. It is recorded as future headroom.

Every operand fact a pass needs (widths, offsets, constants) is an
immediate in the instruction, so passes rewrite `Ins` sequences without a
format change.

## Semantics the ISA must preserve

Each item names the mechanism that preserves it.

- **Owner hold.** A `when` or `match` with no matching clause must hold
  the previous value. `expr.rs` lowers the missing clause to a
  self-reference (`Referent::Owner`). The resolver maps `Owner` to the
  owner's own `SignalId`, and `Load` reads the live value. Identical to
  today's `Context::get(Owner)`.
- **Mid-propagation register transfer.** `flow`'s edge-detect and
  transfer loop is the same code before and after the rewrite. The VM
  produces only the new value. The dirty list preserves today's LIFO
  order bit for bit, so the point where a rising clock is observed does
  not move. `transfer_clock` (already landed) is the single
  implementation shared by `flow` and `set_without_flow`.
- **Socket aliases.** Kept as a runtime copy in `flow`, not compiled to
  instructions, so the clock-through-a-socket case stays in one
  auditable place.
- **Initials and `flow_all`.** Initial expressions compile through the
  same pipeline and run once. `flow_all` iterates every driven component
  through the executor to a fixpoint, then clears the dirty list.
- **Watchers and VCD.** The watcher table, the quiescence loop, and the
  outermost-unlock-only rule are untouched. VCD tracing is watcher-based
  and needs no change.
- **Builtins.** `builtins::attach` (regfile, RAM, CSRs) is a pure user of
  the public API and needs no change.
- **Match arm padding.** `decode_ctor` returns `None` when a valid tag
  has dirty padding, which makes the arm not taken today. The tag-compare
  lowering must reproduce this. Recorded in the layout accessor's docs
  and in the compile checklist.
- **Unresolved referents.** Today an unresolved referent logs a debug
  line and evaluates as `Bit(false)`. The compiler logs the line once at
  compile time and emits a constant-false load. No behavior change.
- **Failure timing.** `Trap` keeps `Method`, `Fn`, and `StrLit` panics at
  first execution rather than moving them to `Sim::new`.

`vm/compile.rs` carries a doc table mapping each instruction to the
`eval_*` function whose semantics it transcribes. Reviewing a lowering
change means walking that table.

## Module map

| File | Status | Owns |
|---|---|---|
| `sim/sim.rs` | keep, edit | `Sim`, `SimLock`, watchers, `attach_clock`. `flow` calls `Vm::run` |
| `sim/circuit.rs` | keep, edit | Construction tables. `exprs` replaced by `program: Program` |
| `sim/expr.rs` | keep | Compiler-only IR (`driver_to_expr`) |
| `sim/eval.rs` | deleted (unit 6) | Was `Context` and the interpreter |
| `sim/value.rs` | landed (unit 6) | `Value`, `InternalValue`, moved verbatim |
| `sim/state.rs` | keep, edit | `vals`, `set_vals`, `transfer`, `set_val`. Dirty list replaced |
| `sim/dirty.rs` | landed | `DirtyList` bitmap + LIFO stack |
| `sim/scheduler.rs` | landed | Min-heap timed queue, FIFO lifecycle queue |
| `sim/layout.rs` | keep, small addition | Pure-geometry accessor for tag and payload offsets |
| `sim/bits.rs` | keep | `BitPat`, unchanged |
| `sim/vm/{isa,frame,exec,compile}.rs` | new | The ISA, frames, executor, compiler |
| `sim/builtins.rs`, `sim/vcd.rs` | untouched | Pure API users |

## Costs that remain, by design

Recorded so no future reader mistakes them for oversights.

- Two scheduler events per clock cycle, because clocking is an
  API-level construct. Each event is now O(log n) heap work.
- One indirect dispatch per executed instruction. No JIT.
- One driver execution per fanout edge of each changed signal per
  settlement. Event-driven sparsity is the point.
- One `Load` per executed reference. It is an indexed `Vec` read on
  demand, not an eager map build of every binding.
- One `Value` compare per `set_val` and `transfer` write. It is the
  termination guarantee.
- A watcher sweep of O(#watchers) per pass, in registration order. No
  pre-filter bitmap. The common case has an empty table.
- Wide values allocate. `Value::Big` and wide slots pay heap costs as
  they did before the rewrite, confined to `*Wide` instructions.
- The clock re-arm pushes a fresh heap entry per half period. It is one
  allocation per half period, not per instruction.

## How the rewrite is verified

The differential harness (`sim-bench/`) is the gate for every unit:

- `simtrace` dumps every elaborated signal, every cycle, in a
  `<cycle> <path> <hex> <bin>` format. Cycle 0 is the initial settled
  state. Later snapshots come from an `on_clock` rising-edge watcher,
  after register transfer and settlement.
- `InitStrategy::Seeded(u64)` makes runs reproducible.
- `sim-bench/golden/` holds committed traces for the generated bench
  designs at 1, 8, 32, 128, and 512 stages, the `assets/new` hello
  design, and `tests/sim/gcd`, all at seed 42 for 16 cycles.
- `bash sim-bench/check.sh` recaptures and diffs. Every migration unit
  must pass it twice before landing.

Performance gates: at least 5x the baseline at both scaling points
(s=1 at 4.5 MHz, s=512 at 0.33 MHz), with actuals reported and the
limiting factor named. `make test` stays green throughout. The old
interpreter was deleted with unit 6, so one engine ships.

## Migration sequence

Each unit lands as one commit on `sim-vm-rewrite`, gated by the harness.

| Unit | Content | Status |
|---|---|---|
| 0 | Bench generator (`sim-bench/gen.py`) | landed, 97c262e |
| 1 | Harness: `Seeded`, `simtrace`, goldens, `check.sh` | landed, 2c021da |
| 2 | `DirtyList`, heap scheduler, `transfer_clock` | landed, 7cff4f2 |
| 3 | ISA, compiler, executor, `flow` wiring, fallback | landed (77c552b) |
| 4 | `when` and `match` lowering; fallback shrinks | landed (2f78a04) |
| 5 | Optimizer passes 1-3 | landed (2525535) |
| 6 | Delete the interpreter; `Value` moves to `value.rs` | landed |
| 7 | Final perf report against both baseline points | pending |

## Alternatives considered

Three candidates were explored in parallel and judged against the same
rubric. The artifacts live with the decision trail of the run that
produced this document.

- **Flat register machine.** One global frame, eager prologue loads,
  doubled S/W opcodes, compile-time rejection of `Method` and `Fn`. A
  close second. Its global frame and eager loads buy dispatch elegance
  the measured floor does not demand. It contributed the
  `transfer_clock` factor, the constant-unit skip, and the differential
  harness plan.
- **Scheduled dataflow kernel.** Compile the whole netlist into one
  topologically ordered stream with clock commit points, Verilator
  style. The best optimizer headroom, but its commit-point constraint is
  cyclic for the everyday `r <= r + 1` register, and it moves evaluation
  order into a scheduler invariant whose hardest case the design did not
  resolve. Right destination if Virdant ever needs that class of
  throughput, wrong base for a semantics-preserving rewrite. It
  contributed `Trap` and the compile checklist.
- **Point fixes on the interpreter.** Pool the `Context`, memoize
  `decode_ctor`. The tree walk and re-encoding remain, and there is no
  artifact for an optimizer to act on. Rejected.
- **Closure compilation and JIT.** Closures are opaque to passes. A JIT
  adds a heavy dependency and an unsafe surface for a problem that is
  allocation churn, not dispatch. Both rejected.

All three candidates independently chose slots over maps, no type tags
inside the machine, bitmap dirty tracking, a heap scheduler, and `Value`
only at commit boundaries. That convergence is the strongest evidence
for the shared core.

## Open questions

- Do nested match patterns (payload constraints inside a pattern) occur
  in practice? ANSWERED (when/match lowering unit): NO.  The `Pat` IR
  (`sim/payload.rs`) is flat (`Ctor` with variable bindings only,
  `WordLit`, `BitLit`, `Else`); `convert_pattern` cannot represent
  payload constraints, and source that looks nested (fifo's
  `case @Valid(in_data) match write[2..0] {...}`) parses as a `Ctor`
  arm whose body is a nested `match` expression.  One `Extract` per
  bound variable is the complete binding rule; no recursive form
  needed.
- Frame memory is O(sum of slots) resident, one frame per driven
  component. ANSWERED (interpreter-deletion unit): moot now that the
  interpreter is gone; the frames are the only per-component state and
  the s=512 bench (30.7M driver evals/run) stays within its measured
  envelope.
- If profiles later show the per-operation width mask matters,
  precompute per-slot masks (a graft deliberately deferred).
- Topological work ordering stays documented headroom. Revisit only with
  a measured need and a semantics argument for the reordering.
