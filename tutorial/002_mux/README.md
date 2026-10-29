# Multiplexers

Now, we will demonstrate how to use conditional expressions through a series of small, related examples.

This lesson's source, `src/mux.vir`, contains all three multiplexers: `Mux2`, `Mux3`, and `Mux4`.

## 2-Way Multiplexer

`Mux2` is a module which takes two inputs and selects between them.

```virdant
mod Mux2 {
    incoming a : Word[8]
    incoming b : Word[8]

    incoming select : Bit

    outgoing out : Word[8] {
        it := mux(select, a, b)
    }
}
```

`Mux2` is a 2-way multiplexer, which takes in two signals `a` and `b` and a control signal `select`.
It outputs `a` if `select` is asserted and `b` otherwise.

This is accomplished through the use of the `mux` primitive.
The first argument to `mux` must have type `Bit`.
The second and third arguments must have the same type as each other.
If the first argument to `mux` is `true`, it returns the second argument.
Otherwise, it returns the third argument.

> **Note**
>
> Verilog uses the syntax `select ? a : b`.
> It is originally borrowed from C,
> and is sometimes called the ternary operator.

## 3-Way Multiplexer

`Mux3` is a module which takes three inputs and selects between them.

```virdant
mod Mux3 {
    incoming a : Word[8]
    incoming b : Word[8]
    incoming c : Word[8]

    incoming select_a : Bit
    incoming select_b : Bit
    incoming select_c : Bit

    outgoing out : Word[8] {
        it := when {
            case select_a => a
            case select_b => b
            case select_c => c
            else => 0
        }
    }
}
```

`Mux3` is a 3-way multiplexer.
It takes three data inputs and three select lines.
The output `out` takes on the value of the *first* of `a` `b` or `c`
whose corresponding select line is asserted
(or `0` if none are).

This is what we would call priority mux.
Multiple values can be selected at once,
but the final result is decided by which has priority.
(Here, `a` has priority over `b` which has priority over `c`).

## When Blocks

In Virdant, we use `when` blocks to represent priority muxes.

```virdant
        it := when {
            case select_a => a
            case select_b => b
            case select_c => c
            else => 0
        }
```

Inside a `when` block is a number of `case` statements.

The syntax `case select_a => a` means:
"if `select_a` is `true`, then the result is `a`"

The condition for each `case` is tested in order until one is found.
Otherwise, the value of the `when` block is given by the `else` clause.

## 4-Way Multiplexer

For our last example, we have `Mux4`,
a 4-way multiplexer which takes four inputs and selects between them.

```virdant
mod Mux4 {
    incoming a : Word[8]
    incoming b : Word[8]
    incoming c : Word[8]
    incoming d : Word[8]

    incoming select : Word[2]

    outgoing out : Word[8] {
        it := match select {
            case 0 => a
            case 1 => b
            case 2 => c
            case 3 => d
        }
    }
}
```

In `Mux3`, each input had its own `select` line.
But `Mux4` encodes which value to select based on a 2-bit.

## Match Blocks

In this example, we chose to make use of final conditional syntax in Virdant:
the `match` block:

```virdant
        it := match select {
            case 0 => a
            case 1 => b
            case 2 => c
            case 3 => d
        }
```

The syntax `match select` looks at `select` and then compares it for equality against each `case`.
When it finds one that matches, the result of the `match` block becomes the value to the right of the `=>`.

One nice thing about `match` is that it prevents you from forgetting to cover every case.
If you forget a case, Virdant will help you out with an error.

`match` statements also produce efficient hardware,
which the cases are tested in parallel.

## Conditionals in Virdant

And so we have seen three different ways to express conditional logic in Virdant:

* the `mux` primitive, for simple two-way selections
* `when` blocks, for priority chains of conditions
* `match` blocks, for comparing a value against all its possible values in parallel

These each give you a different way to express yourself in Virdant.
Some situations call for one kind of expression,
and some call for another.

Ultimately, it's up to you to decide which you like best for a given situation.
Which form will you reach for first?
