# Idioms

Canonical, copyable patterns.

## Conventions used in every example

* A module that declares exactly one `Clock` component omits the `on` clause on `reg`s.
  Writing the redundant `reg r : T on clock` there compiles but emits a WARNING.
* One statement per line; 4-space indent; no tabs (see `./coding-conventions.md`).
* `//>` docstrings on declarations; `//!` package docstring on line 1 of a file
  (see `./coding-conventions.md`).
* The `.vir` file stem becomes the package name, so it must be a valid identifier:
  snake_case (`my_module.vir`), never `my-module.vir` (hyphen is a compile error).
* `pub` on any item another package references.

## FSM: match on the register itself

* Model state as a `union type` when states carry payloads, `enum type` otherwise (see `./types-and-patterns.md`).
* Set the power-on state with `initial it <=` inside the `reg` block.
* Compute the next state by matching on the register's own value: `it <= match it : State { ... }`.
* Guard an arm with a nested `if { when ... => ... else => ... }`.
* Arms are checked in order; the match must be exhaustive (one arm per union variant, or an `else`).
* If the FSM transition table is large, you may want to *not* have it inside of a driver block for readability.

```virdant
//! Fsm example package.

union type FsmState {
    Idle()
    Count(n : Word[2])
    Wrapped()
}

mod Walker {
    incoming clock : Clock

    outgoing state : FsmState

    reg walk : FsmState {
        initial it <= @Idle()
        it <= match it : FsmState {
            with @Idle() => @Count(0)
            with @Count(n) if {
                when n == 3 => @Wrapped()
                else => @Count(n + 1)
            }
            with @Wrapped() => @Wrapped()
        }
    }

    state := walk
}
```

## FSM: one-hot enum + statement-level `if`

* Use an `enum type` when states carry no payload.
* Enumerants with no explicit values are one-hot encoded; if you give explicit
  values, you must give every enumerant one.
* Reference enumerants with `#Name`.
* When several registers change together (phase AND timer), drive them from one
  statement-level `if` and precompute the next state in wires (see
  `./control-flow.md`, `./driver-blocks.md`).

```virdant
enum type Phase {
    Green = 1w4
    Yellow = 2w4
    Red = 4w4
    Walk = 8w4
}

mod Light {
    incoming clock : Clock
    incoming walk_req : Bit

    outgoing walk_signal : Bit

    reg phase : Phase
    reg timer : Word[8]

    walk_signal := match phase { with #Walk => true else => false }

    wire next_phase : Phase
    next_phase := match phase {
        with #Green  => #Yellow
        with #Yellow => #Red
        with #Red    => mux(walk_req, #Walk, #Green)
        with #Walk   => #Green
    }

    wire next_timer : Word[8]
    next_timer := match next_phase {
        with #Green  => 10
        with #Yellow => 3
        with #Red    => 8
        with #Walk   => 5
    }

    if {
        when timer == 0 {
            phase <= next_phase
            timer <= next_timer
        }
        else {
            timer <= timer - 1
        }
    }
}
```

## `outgoing reg` and `outgoing wire`

- Whenever you would ever declare an `outgoing out` and then a `reg out_r`
  and then connect them as `out := out_r` to buffer an output,
  instead, combine the two and just use `outgoing reg out`.
- You cannot read from an `outgoing` (It's a ReadFromSink warning).
  If you ever find yourself needing to, instead, change it to an `outgoing wire`.

## Handshake: `Valid[T]` payloads over `Pipe[T]`

* Use the builtin `Valid[T]` enum: `@Valid(d)` to offer data, `@Invalid()` when idle.
* Use the builtin `Pipe[T]` socket: the producer (client side) drives `data`,
  the consumer (server side) drives `ready`.
* Connect a client socket to a server socket with `:=:` (see `./sockets.md`).
* The "fires" idiom: a transfer fires when the payload is `@Valid` AND the peer
  is ready.  Use `@Valid(dontcare)` when the payload value is irrelevant.
* Gate `ready` on available capacity; advance bookkeeping counters only in
  cycles where a transfer actually fires.

```virdant
//! Producer/consumer over the builtin Pipe socket.

mod Client[W : Type] {
    incoming d : W

    client socket pipe of Pipe[W] {
        it.data := @Valid(d)
    }

    outgoing ready : Bit
    ready := pipe.ready
}

mod Server[W : Type] {
    server socket pipe of Pipe[W] {
        it.ready := true
    }

    outgoing data : Valid[W]
    data := pipe.data
}

mod Connected {
    mod c8 of Client[Word[8]] {
        it.d := 0b1010_0011
    }
    mod s8 of Server[Word[8]] {
        c8.pipe :=: it.pipe
    }
}
```

### Backpressure: the skid-buffer shape

* Keep `ready` high while a free slot exists (`count != 2`), so an incoming
  transfer during a downstream stall lands in the skid register and is never dropped.
* The head (`slot0`) is what is presented downstream; `slot1` catches the
  transfer that arrives while the head is stalled.

```virdant
//! Two-deep register-based buffer with Pipe handshakes on both sides.

mod SkidBuffer {
    incoming clock : Clock

    server socket inp of Pipe[Word[8]] {
        it.ready := count != 2
    }

    client socket out of Pipe[Word[8]] {
        it.data := out_data
    }

    wire out_data : Valid[Word[8]] {
        it := if {
            when count == 0 => @Invalid()
            else => @Valid(slot0)
        }
    }

    wire in_word : Word[8] {
        it := match inp.data {
            with @Valid(data) => data
            with @Invalid() => 0
        }
    }

    wire inp_fires : Bit {
        it := match inp.data {
            with @Valid(dontcare) => count != 2
            with @Invalid() => false
        }
    }

    wire out_fires : Bit {
        it := match out_data {
            with @Valid(dontcare) => out.ready
            with @Invalid() => false
        }
    }

    reg slot0 : Word[8] {
        initial it <= 0
        if {
            when inp_fires && count == 0 {
                it <= in_word
            }
            when inp_fires && out_fires && count == 1 {
                it <= in_word
            }
            when out_fires && count == 2 {
                it <= slot1
            }
        }
    }

    reg slot1 : Word[8] {
        initial it <= 0
        if {
            when inp_fires && count == 1 && !out_fires {
                it <= in_word
            }
        }
    }

    reg count : Word[2] {
        initial it <= 0
        if {
            when inp_fires && !out_fires {
                it <= it + 1
            }
            when out_fires && !inp_fires {
                it <= it - 1
            }
        }
    }
}
```

## Debouncer and edge detector

* Debounce by counting consecutive stable cycles and latching only at the threshold.
* Compute the counter's next value combinationally (`next_count`) so the
  current cycle's input is included in the stability check.
* An edge detector is a registered previous value compared with the input.

```virdant
//! Debounces `inp`: `out` settles only after `inp` has held steady
//! for 3 consecutive cycles.

mod Debouncer {
    incoming clock : Clock

    incoming inp : Bit

    outgoing out : Bit {
        it := stable
    }

    reg prev : Bit {
        initial it <= false
        it <= inp
    }

    reg count : Word[2] {
        initial it <= 0
        it <= next_count
    }

    reg stable : Bit {
        initial it <= false
        if {
            when next_count == 3 {
                it <= inp
            }
        }
    }

    wire next_count : Word[2] {
        it := if {
            when inp == prev => mux(count == 3, count, count + 1)
            else => 1
        }
    }
}

// Any-edge detector: one-cycle pulse on every transition of `inp`.

mod EdgeBoth {
    incoming clock : Clock

    incoming inp : Bit

    outgoing edge : Bit dependson inp {
        it := last != inp
    }

    reg last : Bit {
        initial it <= false
        it <= inp
    }
}
```

For a rising edge only: `edge := !last && inp`.  For a falling edge only: `edge := last && !inp`.

## Counters

* Wrap at a non-power-of-2 modulus with `mux`.
* There is no `>>` operator: implement `gray := bin ^ (bin >> 1)` with
  `word()` and an exclusive-top slice (see the slicing table below).

```virdant
//! Counts 0 through 9, then wraps back to 0.

mod CounterMod10 {
    incoming clock : Clock

    outgoing count : Word[4]

    reg value : Word[4] {
        initial it <= 0
        it <= mux(it == 9, 0, it + 1)
    }

    count := value
}

// Gray code of a free-running binary counter.

mod GrayCounter {
    incoming clock : Clock

    outgoing gray : Word[4] {
        it := bin ^ word(0w1, bin[4..1])
    }

    reg bin : Word[4] {
        initial it <= 0
        it <= it + 1
    }
}

// Johnson (twisted ring) counter: shifts right and inserts the
// complement of the LSB at the MSB.

mod JohnsonCounter {
    incoming clock : Clock

    outgoing state : Word[4]

    reg bits : Word[4] {
        initial it <= 0
        it <= word(!it[0], it[4..1])
    }

    state := bits
}
```

## Barrel shifter and variable shifts

* Build an N-stage barrel shifter: one `mux` per shift-amount bit, each stage
  a conditional `word()` shift (by 4, then 2, then 1 for a 3-bit amount).
* For a variable right shift over a narrow type, a `match` on the shift
  amount with one fixed slice per arm is clear and exhaustive.

```virdant
//! 8-bit logical left barrel shifter from 3 stages of
//! conditional `word()` shifts.

mod BarrelShifter {
    incoming x : Word[8]
    incoming s : Word[3]

    outgoing shifted : Word[8] dependson x, s

    wire stage4 : Word[8] {
        it := mux(s[2], word(x[4..0], 0w4), x)
    }

    wire stage2 : Word[8] {
        it := mux(s[1], word(stage4[6..0], 0w2), stage4)
    }

    wire stage1 : Word[8] {
        it := mux(s[0], word(stage2[7..0], 0w1), stage2)
    }

    shifted := stage1
}

// Variable logical right shift, via match + exclusive-top slices.

mod VariableShr {
    incoming x : Word[4]
    incoming s : Word[3]

    outgoing shifted : Word[4] dependson s, x {
        it := match s {
            with 0 => x
            with 1 => word(0w1, x[4..1])
            with 2 => word(0w2, x[4..2])
            with 3 => word(0w3, x[4..3])
            else => 0
        }
    }
}
```

## Arithmetic: carry, borrow, three-way compare

* `+` and `-` wrap at the operand width.
* `+.` and `-.` produce a result one bit wider than the operands; the extra
  MSB is the carry-out (`+.`) or borrow-out (`-.`).
* Unwrap with a slice: for `sum : Word[5]`, `sum[4..0]` is the low `Word[4]`
  and `sum[5]` is the carry/borrow bit.
* Chain `when` arms in an `if` expression for a three-way comparator; keep an
  `else` for exhaustiveness (see `./control-flow.md`).

```virdant
mod CarryBorrow {
    incoming clock : Clock

    reg count : Word[4] {
        initial it <= 0
        it <= it + 1
    }

    wire sum : Word[5] {
        it := count +. 0xFw4
    }

    wire diff : Word[5] {
        it := count -. 0x1w4
    }
}

mod ThreeWay {
    incoming x : Word[2]
    incoming y : Word[2]

    outgoing code : Word[2] dependson x, y {
        it := if {
            when x < y => 0
            when x == y => 1
            when x > y => 2
            else => 3
        }
    }
}
```

## Submodule wiring: fanout forwarding and constants

* Fan one incoming port out to several submodules from the declaration site
  with a forwarding block; `it` in that block is the port itself.
* Tie child inputs to constants or literals directly at the instance.

```virdant
//! Free-running counter advancing by `step` every clock edge.

mod FanCounter {
    incoming clock : Clock

    incoming step : Word[8]

    outgoing count : Word[8]

    reg value : Word[8] {
        initial it <= 0
        it <= it + step
    }

    count := value
}

pub mod Top {
    incoming clock : Clock {
        a.clock := it
        b.clock := it
    }

    mod a of FanCounter {
        it.step := 1
    }

    mod b of FanCounter {
        it.step := 2
    }
}

mod ThresholdUnit {
    incoming offset : Word[8]
    incoming threshold : Word[8]
    incoming value : Word[8]

    outgoing total : Word[8] dependson offset, value {
        it := value + offset
    }

    outgoing under : Bit dependson offset, threshold, value {
        it := total < threshold
    }
}

mod Parent {
    incoming clock : Clock

    mod unit of ThresholdUnit {
        it.threshold := 7
        it.offset := 3
        it.value := value
    }

    reg value : Word[8] {
        initial it <= 0
        it <= it + 1
    }
}
```

Self-checking test tops (`pub mod Top for virt::Virt` with `exit : Valid[Word[8]]`)
are a tooling pattern, not an idiom, and are out of scope for this file.

## Async

Whenever a port comes from the outside world and is likely *not* synchronized with a clock
(eg, buttons, switches, UART rx, USB D+ and D-, etc, etc), you should synchronize it with the clock
using builtin `Sync` module.

## Virdant vs Verilog

| Verilog | Virdant | Notes |
| --- | --- | --- |
| `x[N-1:M]` | `x[N..M]` | Top index is EXCLUSIVE. Width is `hi - lo`: `x[7..4]` is `Word[3]` (7 - 4). |
| `x[3:0]`, full slice | `x[4..0]` | Width is 4 = 4 - 0. `x[0]` is the LSB. |
| `x << s`, `x >> s` | `word()` + slices | No shift operators. Left: `word(x[4..0], 0w4)`. Right: `word(0w1, x[4..1])`. Variable amounts: barrel shifter or `match` on the amount. |
| `always @(posedge clk)` | driver blocks | No always blocks. Continuous drives `:=` on wires, clocked drives `<=` on regs, guarded by statement-level `if`/`match` (see `./driver-blocks.md`). |
| `<=` usable anywhere | `<=` on regs ONLY | `:=` for wires, ports, and sockets. `<=` is the clocked (non-blocking) assignment and is a compile error on anything but a `reg`. |
| Enumerations default to sequential encoding | one-hot enum default | Enumerants with no explicit values get `1 << i` and the enum is as wide as it has enumerants, so all-zeros is never a valid value. Explicit values must be all-or-none. |
| (no equivalent) | `dependson` contracts | An `outgoing` port declares which components it may read. Reading an unlisted input is an error; an unused `dependson` entry is a warning. Applies to ports only, never to internal wires/regs/nets, and not to tristate components. |
| `assign sda = pull ? 0 : 1'bz` | `bus`/`net` + `#=`, `#-` | `bus` is a bidirectional port, `net` an internal tristate line. `#=` drives strongly, `#-` weakly (pull resistor), `#=#` attaches a submodule bus port to a line. Reads go through `tap`: `seen := tap sda`. |
| `.field` on structs | `->` projection | Struct fields are read with `->`: `r := color->red`. Left-associative: `outer->inner->field`. Dot (`.`) is reserved for module ports and instances. |

## Reserved keywords

The following words are reserved and may NOT be used as identifiers
(component, module, type, instance, wire, reg, ... names).  Naming a
component `width`, or a module `Socket`, is a compile error:

| | | | | | |
| --- | --- | --- | --- | --- | --- |
| `builtin` | `bus` | `client` | `cosi` | `default` | `dependson` |
| `dontcare` | `dyn` | `else` | `enum` | `export` | `ext` |
| `false` | `fn` | `for` | `if` | `import` | `incoming` |
| `initial` | `it` | `let` | `match` | `mod` | `net` |
| `of` | `on` | `outgoing` | `platform` | `pub` | `reg` |
| `server` | `soci` | `socket` | `submod` | `struct` | `tap` |
| `true` | `type` | `union` | `unused` | `when` | `width` |
| `wire` | `with` | | | | |
