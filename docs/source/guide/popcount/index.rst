Population Count
================
In this example, we will learn about modules which contain other modules!

Our example will contain three module definitions:
`PopCount2` `PopCount4` and `PopCount8`.
These each perform a population count, or pop count for short.
The pop count of a number is how many `1`'s there are
in the binary representation of the number.

We will look at each module in turn,
and then we will see the full example at the end.

.. note::

    Most processors have a dedicated population count instruction:

    - `POPCNT` on x86
    - `CNTPOP` on ARM
    - `CPOP` on RISC-V

    These are important for certain low-level applications,
    especially in cryptography, graphics, and networking.


2-bit Pop Count
---------------
The smallest of our three modules is `PopCount2`,
which operates on a 2-bit input:

.. literalinclude:: src/popcount.vir
    :language: virdant
    :lines: 1-12
    :dedent:

The output, `out` is also a 2 bit value,
since it needs to be able to represent `0` `1` or `2`.

`match` Expressions
-------------------
To implement the logic of `PopCount2`,
we make use of the fact that `x` can only take on four values.
Written in binary, these are:
 `0b00` `0b01` `0b10` and `0b11`.

We use a `match` expression to look at the value of `x` and assign the answer:

.. literalinclude:: src/popcount.vir
    :language: virdant
    :lines: 5-10
    :dedent:

The `match x` part means "look at the value of `x`.
That value is then matched with one of the patterns.
Each pattern is written with the keyword `with`.

Once we find the matching pattern,
the result of the whole expression becomes
what's written on the right hand side of the arrow `=>`.

`PopCount2` is a brute force approach that simply looks up the answer in a table.
Look-up tables, which are often called :tooltip:`LUT (Look-up Table)` in hardware,
are a simple and useful tool for many tasks.
`match` expressions are a convenient way of expressing them (among other things!).

One fact to keep in mind: `match` statements are nice because they run in parallel.
Unlike in programming, where `if` `else if` and `else` blocks are checked in sequence.
our `match` statement, running on an FPGA, will try matching `x` against *all* patterns
at the same time.

4-bit Pop Count
---------------
Now for the full 4-bit version, `PopCount4`:

.. literalinclude:: src/popcount.vir
    :language: virdant
    :lines: 14-28

In this case, our 4-bit input `x` could have any of 16 possible values.
We could still use a `match` statement and implement our logic as a lookup table.
But it might be nicer to use a different strategy:
divide and conquer.

Slices
------
First, let's focus on two key expressions:
`x[2..0]` and `x[4..2]`.
These split `x` in half.

The notation `x[i..j]` means "take `x` and and take the bits `i-1` `i-2` ... `k`.
So `x[2..0]` gives us bit 1 and bit 0 of `x`,
and `x[4..2]` gives us bit 3 and bit 2 of `x`.

.. note::

    The indexes must be constants.
    If you want to dynamically index into a `Word[n]`,
    you have to write `x[dyn i]`.

Be careful that the high bit index comes first and is *excluded*,
but the low bit index comes second an is *included*.

.. warning::

    This will seem weird if you're coming from a programming background,
    because the high index comes first instead of second.

    This will seem weird if you're coming from a Verilog background,
    because the top index is excluded.


The type of the result of a slice is easy to see: `x[2..0]` is 2 - 0 = 2 bits wide.
`x[4..2]` is 4 - 2 = 2 bits wide as well.
So both `x[2..0]` and `x[4..2]` have type `Word[2]`.

But what should we do with a pair of `Word[2]`\s?...

Oh.
We could use `PopCount2` to count their 1 bits!


Submodules
----------
To count the 1 bits in each of half of `x`,
we instantiate two submodules:

.. literalinclude:: src/popcount.vir
    :language: virdant
    :lines: 17-19
    :dedent:

The `mod lower` keyword says we're instantiating a submodule named `lower`.
It is `of PopCount2` because it will be an instance of `PopCount2`.

When we instantiate a submodule, we get access to its ports.
`PopCount2`'s `x` port is an `incoming` port,
which means its parent (that's us) needs to drive a value.
So we do so, driving it with `x[2..0]`,
the lower half of `x` (thus the name `lower`).

.. note::

    We could have written this as `lower.x`,
    but because we're inside a driver block,
    we use the shorthand `it.x` instead.

    Driver blocks save us a lot of tping when we have half a dozen ports!

The second submodule, `upper`, is pretty much the same:

.. literalinclude:: src/popcount.vir
    :language: virdant
    :lines: 21-23
    :dedent:

Once we satisfy the submodules by driving their `incoming` ports,
we get to use their `outgoing` ports.
That is, `lower.count` is now the population count of the lower half of `x`,
while `upper.count` is that of the upper half.

What should we do with `lower.count` and `upper.count`?

Addition with Carry
-------------------
We've already met the `+` operator, which adds two numbers together.

The `+.` operator is similar,
but the result is one bit larger than the bigger of the two operands.
It lets us keep track of the carry bit.

.. note::

    The `.` in `+.` is supposed to remind us of the "extra bit"
    needed to represent the carry.

If we were to use `+` instead,
the case of `x` = `0b1111` would give us
`lower.count + upper.count` as `2 + 2`.
The result *should* be a 3-bit number, telling us there are 4 bits.
But instead, it would wrap around, giving us zero.

.. literalinclude:: src/popcount.vir
    :language: virdant
    :lines: 41-43
    :dedent:

What happens if we accidentally used `+` instead of `+.`?
Don't worry.
Virdant would see we are trying to drive `count`, which has type `Word[3]`,
with a value `upper.count + lower.count`, which has type `Word[2]`,
and the result would be a friendly diagnostic message telling you
that you can't do that.

.. note::

    Virdant catches a *lot* of would-be errors this way.
    For example, in the `match` statement from `PopCount2`,
    if you forget a possible pattern in the `with` statement,
    it will tell you.

    World-class diagnostics are just one of several ways Virdant lets you focus on your design.

.. tip::

    If you use VSCode, NeoVim, or any code editor which supports the Language Server Protocol,
    :doc:`Virdant's Language Server </tools/ls>` lets you see
    immediate inline diagnostics in your code.


Scaling Up
----------
From here, the same recipe scales to any width.

Let's look at `PopCount8`:

.. literalinclude:: src/popcount.vir
    :language: virdant
    :lines: 30-44
    :dedent:

The only differences between `PopCount4` and `PopCount8` are:

- the two halves of `x` are bigger (4-bits instead of 2), and
- the `count` at the end is bigger (4-bits istead of 3)

That's it.

If you wanted to write `PopCount16`, how would you do it?

Full Example
------------
The full example with all three modules is presented here:

.. literalinclude:: src/popcount.vir
    :caption: popcount.vir
    :language: virdant
    :class: small-code


Waveform
--------
Let's give each module its own simulation.

For `PopCount2`, we will look at all possible inputs:

.. vcd:: trace2.vcd

    label Input
    signal top.x:bin

    label Count
    signal top.count:dec

Nothing mysterious here:
for `x` = `0b00` the count is `0`,
for `x` = `0b01` the count is `1`,
for `x` = `0b10` the count is `1`,
and for `x` = `0b11` both bits are set and the count is `2`.

For `PopCount4`, we'll just look at a sample of the 16 possible inputs:

.. vcd:: trace4.vcd

    label Input
    signal top.x:bin

    label Pair Counts
    signal top.lower.count:dec
    signal top.upper.count:dec

    label Count
    signal top.count:dec

Finally, for `PopCount8` we have this:

.. vcd:: trace8.vcd

    label Input
    signal top.x:bin

    label Half Counts
    signal top.lower.count:dec
    signal top.upper.count:dec

    label Count
    signal top.count:dec
