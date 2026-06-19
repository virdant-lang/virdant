---
name: virdant
description:
    Guidance when writing or editing .vir (Virdant) source files.
    Virdant coding conventions.
---

# Virdant Language Tutorial

https://virdant.org/tutorial/

# Virdant Language Reference

https://virdant.org/reference/

# Coding Conventions

* `./coding-conventions.md`

# Techniques

* `./driver-blocks.md`
* `./control-flow.md`
* `./sockets.md`
* `./types-and-patterns.md`
* `./literals-and-builtins.md`
* `./module-system.md`

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
