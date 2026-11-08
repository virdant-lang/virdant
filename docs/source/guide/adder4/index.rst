4-bit Adder
===========
The joy of the `FullAdder` is that they can be chained together to add numbers of any size.

We define a module `Adder4`, which can add together any two 4-bit numbers:

.. literalinclude:: /guide/adder/src/adder.vir
    :language: virdant
    :lines: 41-80

.. note::

    This is the last in a series of three modules which appear in full in :doc:`../adder/index`.

A quick survey of the code shows us that there are three `incoming` ports:
`a` `b` and `carry_in`,
and two `outgoing` ports:
`sum` and `carry_out`.

However, this time, `a` `b` and `sum` are all `Word[4]`,
which means we can pass any 4-bit number.
This allows us to add any two numbers between 0 and 15.

The body consists of four `FullAdder` submodules.
But let's not get ahead of ourselves.
Let's take a closer look.

Indexing
--------
As we look at our first submodule, `fulladder_0`, find ourselves in a pickle:
Both `a` and `b` have type `Word[4]`.
But the inputs to `FullAdder` (also named `a` and `b`) have type `Bit`.
How do we connect them together?

The answer is we need to decompose `a` and `b` into their constituent bits.
The syntax `a[0]` means "from the word `a`, extract bit number `0`".

When adding numbers by hand, we line up the digits of the two numbers vertically.
In the same way, in `Adder4`, we line up corresponding pairs of bits:

- `a[0]` and `b[0]`
- `a[1]` and `b[1]`
- `a[2]` and `b[2]`
- `a[3]` and `b[3]`

Consult this slightly messy diagram for an idea of what goes where:

.. inline-svg:: guide/adder4/adder4.svg
    :align: center

.. note::

    It's important to note that indexing like this only works
    when the index is a constant.
    If you need dynamic indexing, Virdant provides a different syntax: `a[dyn i]`.

    Dynamic indexing is more expensive in terms of hardware.
    And because it can otherwise lead to undefined behavior,
    Virdant restricts it to only work for `Word[n]`
    where `n` is a power of 2.


Chaining Full Adders
--------------------
The operation of `Adder4` is based on the four `FullAdder` which are all chained together.

.. literalinclude:: /guide/adder/src/adder.vir
    :language: virdant
    :lines: 61-65
    :dedent:

Each `FullAdder` takes the corresponding pair of bits from `a` and `b`.
Its `carry_in` is the `carry_out` of the previous `FullAdder`.
(The `carry_in` of the first, of course, is the `incoming` port `carry_in`).

Each `FullAdder` represents a column of the addition.
The reason you need a chain is exactly because the "carry" bit
carries over to the next column.


Concatenation
-------------
The opposite of indexing is concatenation.
This is used on the last line of `Adder4`
to produce the final result.

Virdant uses the syntax `word(a, ..., z)`
to concatenate the values `a` `b` `c` ... `z` together.
You can concatenate as many values as you like,
and the values you concatenate can be `Bit`\s or `Word`\s.

The result of `word(a, ..., z)` will be a `Word` type.
And its size will be computed based on the sum of the sizes of each of the arguments.

For example, because each of the arguments is of type `Bit`,
the multi-line expression

.. code-block:: virdant

    word(
        fulladder_3.sum,
        fulladder_2.sum,
        fulladder_1.sum,
        fulladder_0.sum,
    )

is `Word[4]`.
This is good, since it's being assigned to `out`, which is also `Word[4]`.


About Adders
------------
The technical name for this style of adder is called a ripple-carry adder.
It is the simplest adder circuit.

It's good to understand that adders are relatively expensive pieces of hardware.
The `HalfAdder` has 2 gates: an AND gate and an XOR gate.
That means the `FullAdder` has 5:
two copies of the `HalfAdder` gives 4 gates,
plus an additional OR gate.
This brings the total cost of the `Adder4` to 4x5 = 20 gates.

If you wanted to build an `Adder64`,
it would require 64 `FullAdder`\s,
and thus 64x5 = 320 gates(!).

Additionally, ripple carry adders can be a bit slow.
Think of it like this: to add two 4-bit numbers,
the `carry_in` has to travel through all four `FullAdder`\s.
(If you work through carefully, it travels through 8 gates.
And each of those takes time.

If you want to do 64-bit addition, though,
that's 64 `FullAdder`\s,
and thus 128 gates it has to pass through.
On an FPGA, this could easily cap your maximum clock frequency.

There are other kinds of adder circuits.
The next most famous one is called the carry lookahead adder.
It does addition much faster,
but at a cost of a higher gate count.

Waveform
--------
I'll leave this waveform for you to look over.

.. vcd:: ../adder/trace.svg

   label Ports
   signal top.a:dec
   signal top.b:dec
   signal top.carry_in
   signal top.sum:dec
   signal top.carry_out
   space

   label Sum
   signal top.fulladder_0.sum
   signal top.fulladder_1.sum
   signal top.fulladder_2.sum
   signal top.fulladder_3.sum
   space

   label Carries
   signal top.fulladder_0.carry_out
   signal top.fulladder_1.carry_out
   signal top.fulladder_2.carry_out
   signal top.fulladder_3.carry_out
