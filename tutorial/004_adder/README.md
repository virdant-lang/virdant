# Adder

In this example, we will learn how to instantiate submodules
and see some more interesting kinds of expressions.

> **Warning**
>
> This example is much longer than the previous ones, but it is not much more difficult.
> Therefore, we will build it up in pieces.
> The complete example is given at the very bottom.

## Half Adder

The first module is `HalfAdder`.
This is a standard circuit everyone should know.

```virdant
mod HalfAdder {
    incoming a : Bit
    incoming b : Bit

    outgoing carry_out : Bit
    outgoing sum : Bit

    // carry is true when a and b are both true
    carry_out := a && b

    // sum is true when one of a and b is true (but not both)
    sum := a ^^ b
}
```

`HalfAdder` takes two 1-bit inputs, `a` and `b`,
and it computes their sum as two 1-bit outputs, `sum` and `carry_out`.

The truth table of `HalfAdder` looks like this:

| `a`     | `b`     | `carry_out` | `sum`   |
|---------|---------|-------------|---------|
| `false` | `false` | `false`     | `false` |
| `false` | `true`  | `false`     | `true`  |
| `true`  | `false` | `false`     | `true`  |
| `true`  | `true`  | `true`      | `false` |

### Logical Operations

If you look carefully at the truth table, `carry_out` is only `true` when *both* `a` and `b` are true.
Therefore, we set `carry_out` equal to `a && b`, the logical AND of `a` and `b`.

Again, looking *very* carefully at the table,
we see `sum` is only `true` when `a` and `b` are opposites of each other.
Therefore, we set `sum` equal to `a ^^ b`, the logical XOR of `a` and `b`.

In both cases, we are driving an `outgoing` port, so we use `:=` and not `<=`.

> **Note**
>
> The other logical operations in Virdant are `a || b` for logical OR and `!a` for logical NOT.
> In fact, we will use `||` a little later!

## Full Adder

Our `HalfAdder` is good enough to add any pair of 1-bit numbers together.
However, if we want to sum up numbers with more than 1 bit, we need to handle carries.

`FullAdder` is a standard circuit which is a variation of `HalfAdder` that does this.

```virdant
mod FullAdder {
    incoming a : Bit
    incoming b : Bit
    incoming carry_in : Bit

    outgoing sum : Bit
    outgoing carry_out : Bit

    // first, sum a + b
    mod ha0 of HalfAdder {
        it.a := a
        it.b := b
    }

    // then, sum the result with carry_in
    mod ha1 of HalfAdder {
        it.a := ha0.sum
        it.b := carry_in
    }

    sum := ha1.sum
    carry_out := ha0.carry_out || ha1.carry_out
}
```

In addition to `a` and `b`, `FullAdder` takes an additional 1-bit input called `carry_in`.
The circuit then adds `a`, `b`, and `carry_in` together and returns their sum as `carry_out` and `sum`.

The way `FullAdder` works is by making use of two `HalfAdder`s!

### First Half Adder Submodule

The first `HalfAdder` is this:

```virdant
    // first, sum a + b
    mod ha0 of HalfAdder {
        it.a := a
        it.b := b
    }
```

The `mod` keyword, when used inside a module definition, creates an *instance* of another module.
Here, we give the submodule the name `ha0`.

> **Note**
>
> As you may have guessed, `ha` stands for "half adder".
> While heavy abbreviation is very common in digital design,
> in Virdant, you are encouraged to use long, descriptive names.
> Perhaps `half_adder0` would have been an even better choice!
>
> Also, it is conventional to start numbering starting at `0`.
> So our "first" `HalfAdder` is not `ha1` but rather `ha0`.

We create a driver block to connect up the ports of `ha0`.
The variables `a` and `b`, of course, refer to the `incoming` ports of our `FullAdder` module.
The `it.a` and `it.b` (or equivalently, `ha0.a` and `ha0.b`) are the `incoming` ports of our `HalfAdder` submodule.
By connecting these as shown, we are passing the inputs `a` and `b` down to the `HalfAdder`.

Once we do this, `ha0` will begin summing `a` and `b` for us.
The sum of `a` and `b` are made available as `ha0.carry_out` and `ha0.sum`.

### Second Half Adder Submodule

Our second `HalfAdder` submodule is this:

```virdant
    // then, sum the result with carry_in
    mod ha1 of HalfAdder {
        it.a := ha0.sum
        it.b := carry_in
    }
```

The difference here is that we are not summing `a` and `b`, but rather `ha0.sum` and `carry_in`.
Since `ha0.sum` equals `a` + `b`, this will have the effect of summing `a` + `b` + `carry_in`, just as we wanted!

### Wrapping up the Full Adder

The two remaining lines of `FullAdder` drive the outputs.

```virdant
    sum := ha1.sum
    carry_out := ha0.carry_out || ha1.carry_out
```

We see the use of a logical OR to compute the `carry_out` in the expression `ha0.carry_out || ha1.carry_out`.

## The 4-bit Adder

The joy of the `FullAdder` is that they can be chained together to add numbers of any size.

We define a module `Adder4`, which can add together any two `Word[4]`s:

```virdant
mod Adder4 {
    incoming a : Word[4]
    incoming b : Word[4]
    incoming carry_in : Bit

    outgoing sum : Word[4]
    outgoing carry_out : Bit

    mod fa0 of FullAdder {
        it.a := a[0]
        it.b := b[0]
        it.carry_in := carry_in
    }

    mod fa1 of FullAdder {
        it.a := a[1]
        it.b := b[1]
        it.carry_in := fa0.carry_out
    }

    mod fa2 of FullAdder {
        it.a := a[2]
        it.b := b[2]
        it.carry_in := fa1.carry_out
    }

    mod fa3 of FullAdder {
        it.a := a[3]
        it.b := b[3]
        it.carry_in := fa2.carry_out
        carry_out := it.carry_out
    }

    sum := word(fa3.sum, fa2.sum, fa1.sum, fa0.sum)
}
```

We will leave some of the details of this module for you to explore,
but here are some features to look out for.

### Indexing

Both `a` and `b` have type `Word[4]`.
But the inputs to our `FullAdder`s is `Bit`.
In order to add corresponding pairs of bits, we need to get access to them.

This syntax `a[0]` means "from the word `a`, extract bit number `0`".

When adding numbers by hand, we line up the digits of the two numbers vertically.
In the same way, in `Adder4`, we line up corresponding pairs of bits:

- `a[0]` and `b[0]`
- `a[1]` and `b[1]`
- `a[2]` and `b[2]`
- `a[3]` and `b[3]`

### Concatenation

The opposite of indexing is concatenation.
This is used on the last line of `Adder4`.

Virdant uses the syntax `word(a, ..., z)` to concatenate the values `a` `b` `c` ... `z` together.
You can concatenate as many values as you like,
and the values you concatenate can be `Bit`s or `Word`s.

The result of `cat(a, ..., z)` will be a `Word` type.
And its size will be computed based on the sum of the sizes of each of the arguments.

For example, because each of the arguments is of type `Bit`,
the expression `word(fa3.sum, fa2.sum, fa1.sum, fa0.sum)` is `Word[4]`.
This is good, since it's being assigned to `out`, which is also `Word[4]`.

### Chaining Full Adders

The key of the `Adder4` design is the four `FullAdder`s which are chained together:

```virdant
    mod fa0 of FullAdder {
        it.a := a[0]
        it.b := b[0]
        it.carry_in := carry_in
    }

    mod fa1 of FullAdder {
        it.a := a[1]
        it.b := b[1]
        it.carry_in := fa0.carry_out
    }

    mod fa2 of FullAdder {
        it.a := a[2]
        it.b := b[2]
        it.carry_in := fa1.carry_out
    }

    mod fa3 of FullAdder {
        it.a := a[3]
        it.b := b[3]
        it.carry_in := fa2.carry_out
        carry_out := it.carry_out
    }
```

To power each `FullAdder`, we must drive its inputs,
sending the corresponding pair of bits to `it.a` and `it.b`.
We also mix in the `carry_out` of the previous `FullAdder` to the `carry_in` of the next.

I wonder what the `carry_in` of the first `FullAdder` should be?

## The Complete Example

```virdant
mod HalfAdder {
    incoming a : Bit
    incoming b : Bit

    outgoing carry_out : Bit
    outgoing sum : Bit

    // carry is true when a and b are both true
    carry_out := a && b

    // sum is true when one of a and b is true (but not both)
    sum := a ^^ b
}

mod FullAdder {
    incoming a : Bit
    incoming b : Bit
    incoming carry_in : Bit

    outgoing sum : Bit
    outgoing carry_out : Bit

    // first, sum a + b
    mod ha0 of HalfAdder {
        it.a := a
        it.b := b
    }

    // then, sum the result with carry_in
    mod ha1 of HalfAdder {
        it.a := ha0.sum
        it.b := carry_in
    }

    sum := ha1.sum
    carry_out := ha0.carry_out || ha1.carry_out
}

mod Adder4 {
    incoming a : Word[4]
    incoming b : Word[4]
    incoming carry_in : Bit

    outgoing sum : Word[4]
    outgoing carry_out : Bit

    mod fa0 of FullAdder {
        it.a := a[0]
        it.b := b[0]
        it.carry_in := carry_in
    }

    mod fa1 of FullAdder {
        it.a := a[1]
        it.b := b[1]
        it.carry_in := fa0.carry_out
    }

    mod fa2 of FullAdder {
        it.a := a[2]
        it.b := b[2]
        it.carry_in := fa1.carry_out
    }

    mod fa3 of FullAdder {
        it.a := a[3]
        it.b := b[3]
        it.carry_in := fa2.carry_out
        carry_out := it.carry_out
    }

    sum := word(fa3.sum, fa2.sum, fa1.sum, fa0.sum)
}
```
