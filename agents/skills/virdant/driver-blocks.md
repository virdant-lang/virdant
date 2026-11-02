Driver Blocks
=============

Use Driver Blocks
-----------------
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


Push in drivers
---------------
Generally speaking, prefer nested drivers inside of `if` and `match` blocks like this:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming data  : Valid[Bit]

    reg bar : Bit on clock {
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

    reg bar : Bit on clock {
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

    reg bar : Bit on clock {
        it <= if {
            when cond1 => false
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

    reg bar : Word[8] on clock {
        it <= mux(cond1, 42, 67)
    }
}
```


Cull redundant register drivers
--------------------------------
Whenever you see this pattern:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming cond2 : Bit

    reg bar : Bit on clock {
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

    reg bar : Bit on clock {
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


Remove unnecessary `=>` when chaining if/match blocks
--------------------------------------------------------
When the body of a `when` or `with` arm is itself an `if` or `match` block,
the grammar lets you chain directly without writing `=>`.

Prefer this (direct chaining, no `=>`):

```virdant
state <= if {
    when reset => @Idle()
    else match state {
        with @Idle() => @Running(x, y)
        with @Running(x, y) if {
            when y == 0 => @Done(x)
            when x < y => @Running(y - x, x)
            else => @Running(x - y, y)
        }
        with @Done(result) => @Idle()
    }
}
```

over this (unnecessary `=>`):

```virdant
state <= if {
    when reset => @Idle()
    else => match state {
        with @Idle() => @Running(x, y)
        with @Running(x, y) => if {
            when y == 0 => @Done(x)
            when x < y => @Running(y - x, x)
            else => @Running(x - y, y)
        }
        with @Done(result) => @Idle()
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
// BAD: unnecessary => before nested when
reg_state <= match state {
    with @Foo() => if {
        when cond => value
    }
}

// GOOD: direct chaining
reg_state <= match state {
    with @Foo() if {
        when cond => value
    }
}
```


`it` self-reference
-------------------
Inside a driver block, `it` can appear on the right-hand side of an
assignment to refer to the current value of the register or wire.
This is how you express "hold" or "modify" logic.

```virdant
reg val : Bit on clock {
    it <= mux(reset, false, !it)     // !it = invert current value
}

reg r : Word[8] on clock {
    it <= mux(reset, 10w8, it + 1)   // it + 1 = increment current value
}
```

This self-reference is distinct from forwarding a submodule's output
(where `it` refers to the submodule instance, not the same signal).
See module-system.md for the instantiation it-block pattern."}]


Register initial values
-----------------------
Use an `initial` statement to give a register a power-on value.
Put it at the top of the register's driver block, using `it`:

```virdant
reg counter : Word[3] on clock {
    initial it <= 7
    it <= it - 1
}
```

Rules:

- The target must be a `reg` or `outgoing reg`.
- The value must be a constant (no component references).
- `initial` is NOT a driver; the register still needs its `<=` driver.
- At most one `initial` per register.
- `initial` may not appear inside an `if` or `match` block.
- Registers without `initial` start at 0 in Verilog (X in the simulator).
