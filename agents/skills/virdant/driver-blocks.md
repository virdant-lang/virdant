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
Generally speaking, prefer nested drivers inside of `when` and `match` blocks like this:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming data  : Valid[Bit]

    reg bar : Bit on clock {
        when {
            case cond1 {
                it <= true
            }
            else match data {
                case @Valid(b) {
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
        it <= when {
            case cond1 => true
            else => match data {
                case @Valid(b) => b
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
        it <= when {
            case cond1 => false
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
        when {
            case cond1 {
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
        when {
            case cond1 {
                it <= cond2
            }
        }
    }
}
```

A `when` block with no arms matching simply leaves the register unchanged,
so the `else { it <= it }` is unnecessary noise.


Remove unnecessary `=>` when chaining when/match blocks
--------------------------------------------------------
When the body of a `when` or `match` arm is itself a `when` or `match` block,
the grammar lets you chain directly without writing `=>`.

Prefer this (direct chaining, no `=>`):

```virdant
state <= when {
    case reset => @Idle()
    else match state {
        case @Idle() => @Running(x, y)
        case @Running(x, y) when {
            case y == 0 => @Done(x)
            case x < y => @Running(y - x, x)
            else => @Running(x - y, y)
        }
        case @Done(result) => @Idle()
    }
}
```

over this (unnecessary `=>`):

```virdant
state <= when {
    case reset => @Idle()
    else => match state {
        case @Idle() => @Running(x, y)
        case @Running(x, y) => when {
            case y == 0 => @Done(x)
            case x < y => @Running(y - x, x)
            else => @Running(x - y, y)
        }
        case @Done(result) => @Idle()
    }
}
```

The direct form applies to all four combinations:

- `case ... when { ... }`
- `case ... match { ... }`
- `else when { ... }`
- `else match { ... }`

It works for statement-level and expression-level when/match.
The extra `=>` adds noise without changing meaning,
so leave it out.

This also applies inside `match` arms:

```virdant
// BAD: unnecessary => before nested when
reg_state <= match state {
    case @Foo() => when {
        case cond => value
    }
}

// GOOD: direct chaining
reg_state <= match state {
    case @Foo() when {
        case cond => value
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
