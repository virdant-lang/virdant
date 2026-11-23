# Driver Blocks

## Use Driver Blocks
Use driver blocks (aka `it` blocks)

Prefer this:

```virdant
mod Foo {
    wire bar : Bit {
        it := true
    }
}
```

instead of this:

```virdant
mod Foo {
    wire bar : Bit
    bar := true
}
```


## Push in drivers
Generally speaking, prefer nested drivers inside of `if` and `match` blocks like this:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming data  : Valid[Bit]

    reg bar : Bit {
        if {
            when cond1 {
                it <= true
            }
            else match data {
                with @Valid(b) {
                    it <= b
                }
            }
        }
    }
}
```

instead of one giant expression like this:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming data  : Valid[Bit]

    reg bar : Bit {
        it <= if {
            when cond1 => true
            else => match data {
                with @Valid(b) => b
                else => it
            }
        }
    }
}
```

However, you may still use the former for smaller cases when it helps readability.
For example, this is fine:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming cond2 : Bit

    reg bar : Bit {
        it <= if {
            when cond1 => false
            when cond2 => false
            else => true
        }
    }
}
```

For simple ternary-like cases, you may also use `mux`:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit

    reg bar : Word[8] {
        it <= mux(cond1, 42, 67)
    }
}
```


## Clock domains

A module's clocking is derived from its *incoming clock ports*: the declared
components typed `Clock` (`incoming clk : Clock`) plus the receiving-side
channels of its socket instances typed `Clock` (see ./sockets.md). Clocks are
*opaque*: a clock value cannot be computed with — it can only be associated
with components (`on` clauses) and passed around.

### Clock classes and aliases

A driver whose target is Clock-typed and whose driving expression is a plain
reference makes the two denote the *same clock class*: clocks are defined by
identity, not assigned.

```virdant
mod ClkBuf {
    incoming clk_in : Clock
    outgoing clk_out : Clock dependson clk_in {
        it := clk_in
    }
}
```

An outgoing Clock port driven from an incoming clock must declare its
provenance with `dependson`; without the declaration the driver is an error
(`MissingClockDependson`). A port with no `dependson` clause — or one whose
definition is rooted at an internal source, e.g. a submodule's clock output —
is a *fresh clock source* and must not be driven.

### The unique clock and `on` clauses

A module's *clock count* is the number of clock sources visible in it:
incoming clock ports, platform-synthesized Clock ports, undriven outgoing
Clock ports, submodule clock outputs (`pll.clkout`), and each `asclock` call
site.

* Clock count exactly one — the module has a *unique clock*: every register
  may omit its `on` clause and is clocked by it implicitly. Writing
  `on clock` anyway is a warning (`RedundantOnClause`, on any component kind
  that can carry an `on` clause) — omit it. The warning is suppressed in a
  module marked `@unsafe_clocks`, where the explicit clause is required.

```virdant
mod Counter {
    incoming clock : Clock

    reg count : Word[8] {
        it <= it + 1
    }
}
```

* Clock count two or more — a *multi-clock module*: every register must name
  its clock explicitly with `on`, and every other component needs an explicit
  `on` or `async` clause.

```virdant
mod Divider {
    incoming clock_a : Clock
    incoming clock_b : Clock

    reg ra : Bit on clock_a {
        it <= !it
    }

    reg rb : Bit on clock_b {
        it <= !it
    }
}
```

* Clock count zero with no `async` component — the module is
  *clock-polymorphic*: its combinational logic is clock-generic and resolves
  to the instantiating context's clock. A zero-clock module that declares any
  `async` component must annotate every component explicitly instead, and no
  zero-clock module can contain registers or memories.

### One clock domain per conditional

A statement-position `if`/`match` containing clock drivers acts on a single
clock domain: its conditions and every clock driver in its arms (an `else`
arm included) must all denote one clock class.

* A guarded driver of a plain clock reference is legal but warns
  (`GuardedClockDriver`): the conditional cannot change which clock the target
  denotes, so the gating is pointless — drive it unconditionally.
* A conditional mixing clock classes is an error (`DomainMismatch`).
* A clock driven by a *computed* expression (e.g. a `mux` over clocks) is an
  error (`ConditionalClockDriver`): clocks are defined, not assigned.

### Clocking an ordinary signal

Use `asclock` to turn a `Bit` into a `Clock` when a register needs to be
clocked by an ordinary signal (see ./literals-and-builtins.md). Each
`asclock` call site produces its own clock class.


## Cull redundant register drivers
Whenever you see this pattern:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming cond2 : Bit

    reg bar : Bit {
        if {
            when cond1 {
                it <= cond2
            }
            else {
                it <= it
            }
        }
    }
}
```

Remove the `else` arm entirely and use the equivalent:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming cond2 : Bit

    reg bar : Bit {
        if {
            when cond1 {
                it <= cond2
            }
        }
    }
}
```

An `if` block with no arms matching simply leaves the register unchanged,
so the `else { it <= it }` is unnecessary noise.
(A driver whose source and target are the same component, such as a
plain `it <= it`, is separately flagged as a redundant driver.)


## Remove unnecessary `=>` when chaining if/match blocks
When the body of a `when` or `with` arm is itself an `if` or `match` block,
the grammar lets you chain directly without writing `=>`.

Prefer this (direct chaining, no `=>`):

```virdant
mod Sequencer {
    incoming clock : Clock
    incoming flag : Bit

    reg state : Word[2] {
        it <= if {
            when flag if {
                when it == 0w2 => 1w2
                else => it
            }
            else match it {
                with 1w2 => 2w2
                else => 0w2
            }
        }
    }
}
```

over this (unnecessary `=>`):

```virdant
mod Sequencer {
    incoming clock : Clock
    incoming flag : Bit

    reg state : Word[2] {
        it <= if {
            when flag => if {
                when it == 0w2 => 1w2
                else => it
            }
            else => match it {
                with 1w2 => 2w2
                else => 0w2
            }
        }
    }
}
```

The direct form applies to all four combinations:

- `when ... if { ... }`
- `when ... match { ... }`
- `else if { ... }`
- `else match { ... }`

It works for statement-level and expression-level if/match.
The extra `=>` adds noise without changing meaning,
so leave it out.

This also applies inside `match` arms:

```virdant
// BAD: unnecessary => before nested if
mod Foo {
    incoming clock : Clock
    incoming flag : Bit

    reg state : Word[2] {
        it <= match it {
            with 0w2 => if {
                when flag => 1w2
                else => 0w2
            }
            else => 2w2
        }
    }
}

// GOOD: direct chaining
mod Foo {
    incoming clock : Clock
    incoming flag : Bit

    reg state : Word[2] {
        it <= match it {
            with 0w2 if {
                when flag => 1w2
                else => 0w2
            }
            else => 2w2
        }
    }
}
```


## `it` self-reference
Inside a driver block, `it` can appear on the right-hand side of an
assignment to refer to the current value of the register or wire.
This is how you express "hold" or "modify" logic.

```virdant
mod Toggle {
    incoming clock : Clock
    incoming reset : Bit

    reg val : Bit {
        it <= mux(reset, false, !it)     // !it inverts the current value
    }
}

mod Incrementer {
    incoming clock : Clock
    incoming reset : Bit

    reg r : Word[8] {
        it <= mux(reset, 10w8, it + 1)   // it + 1 increments the current value
    }
}
```

This self-reference is distinct from forwarding a submodule's output
(where `it` refers to the submodule instance, not the same signal).
See ./modules.md for the instantiation it-block pattern.


## Register initial values
Use an `initial` statement to give a register a power-on value.
Put it at the top of the register's driver block, using `it`:

```virdant
mod DownCounter {
    incoming clock : Clock

    reg counter : Word[3] {
        initial it <= 7
        it <= it - 1
    }
}
```

Rules:

- The target must be a `reg` or `outgoing reg`.
- The value must be a constant (no component references).
- `initial` is NOT a driver; the register still needs its `<=` driver.
- At most one `initial` per register.
- `initial` may not appear inside an `if` or `match` block.
- Registers without `initial` start at an unspecified value: the simulator
  gives them a pseudorandom bit pattern. If a register's power-on value
  matters, give it an explicit `initial` statement.
- Some platforms forbid `initial` statements with the `@noinitial`
  annotation; see ./platforms.md.


## Driver warnings
* `empty_driver_block`: an it-block containing no drivers warns.
* `no_reg_drivers`: a `reg` or `outgoing reg` with no `<=` driver warns,
  even if it has an `initial` statement (which is not a driver).
