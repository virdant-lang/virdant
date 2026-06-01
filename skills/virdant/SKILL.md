---
name: virdant
description:
    Guidance when writing or editing Virdant files.
    Virdant coding conventions.
---

# Virdant Language Tutorial

https://virdant.org/tutorial/

# Virdant Language Reference

https://virdant.org/reference/

# Coding Conventions

See ./coding-conventions.md

# Techniques

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

Generally speaking, prefer nested drivers inside of conditionals like this:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming data  : Valid[Bit]

    reg bar : Bit on clock {
        if cond1 {
            it <= true
        } else {
            match data {
                @Valid(b) {
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
        it <= if cond1 {
            true
        } else {
            match data {
                @Valid(b) {
                    b
                }
                @Invalid() {
                    it
                }
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
        it <= if cond1 {
            false
        } else {
            true
        }
    }
}
```

## Cull redundant register drivers

When ever you see this pattern:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming cond2 : Bit

    reg bar : Bit  on clock {
        if cond1 {
            it <= cont2
        } else {
            it <= it
        }
    }
}
```

Remove the `it <= it` driver and use the equialent:

```virdant
mod Foo {
    incoming clock : Clock
    incoming cond1 : Bit
    incoming cond2 : Bit

    reg bar : Bit  on clock {
        if cond1 {
            it <= cont2
        }
    }
}
```

# Unused variables

Make sure to use `unused` inside the driver block to suppress unused variable warnings.

```virdant
mod Foo {
    outgoing out_true : Bit {
        it := true
    }

    outgoing out_false : Bit {
        it := false
    }
}

mod Bar {
    outgoing out : Bit

    mod foo of Foo {
        unused it.out_false
        out := it.out_true
    }
}
```

# Bit slicing

This is VERY IMPORTANT!:

Bit slicing in Virdant is different from Verilog:

    Verilog: `x[31:0]` is bits 31 down to bit 0
    Virdant: `x[32..0]` is bits 31 down to bit 0

In general, `x[N-1:M]` in Verilog becomes `x[N..M]` in Virdant.

The top index (eg, the first index) in a bit slice is EXCLUSIVE.
`x[4..1]` is the same as `word(x[3], x[2], x[1])`
You can double check that `x[4..1]` is type `Word[3]` and we know this because 4 - 1 = 3.

Another example: x[32..20] is 32-20 = 12 bits wide, so it is `Word[12]`.
