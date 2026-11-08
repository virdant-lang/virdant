Multiplier
==========
Time for the crown jewel of the arithmetic lessons: multiplication.

Multiply two 4-bit numbers together,
and the answer can be as large as 15 x 15 = 225.
Our `Multiplier4` takes two `Word[4]`\s
and produces their product as a `Word[8]`.

.. literalinclude:: /guide/multiplier/src/multiplier.vir
    :language: virdant
    :lines: 6-50

The whole module is only about twenty lines of real content,
because we are going to lean on ideas you already have:
masking, concatenation, and the widening add.

We'll build it the same way you would multiply on paper.

Multiplication on Paper
-----------------------
Think back to multiplying by hand.
To compute 123 x 45, you multiply 123 by each digit of 45 --
then shift each partial product into position and add them up.

.. code-block:: none

      123
    x  45
    -----
      615      <- 123 x 5
     4920      <- 123 x 4, shifted one place left
    -----
    5535

Binary multiplication works exactly the same way,
except the "digits" are bits, and that makes life wonderfully easy.
There are only two possibilities for each digit:

* if the bit of `a` is `0`, the partial product is zero
* if the bit of `a` is `1`, the partial product is a copy of `b`

And an operation that "keeps everything or keeps nothing"
has a name you already know:
it is the AND operation.

Partial Products
----------------
Here is the first row of our paper multiplication,
written in Virdant:

.. literalinclude:: /guide/multiplier/src/multiplier.vir
    :language: virdant
    :lines: 19-22
    :dedent:

Let's take this apart.
The expression `word(a[0], a[0], a[0], a[0])` copies the single bit `a[0]`
four times, producing a `Word[4]` filled with `a[0]`.
We then AND it with `b`.

The result is `b` itself wherever `a[0]` is set,
and 0 wherever `a[0]` is clear.
We call this a **partial product**:
it is `b` "weighted" by one bit of `a`.

And here is something new: `row0` is a `wire`.
An `outgoing` sends a value to the outside world,
but a `wire` is a value that lives *inside* the module --
a scratchpad for intermediate results.
Driver blocks work just as they always have:
`it` refers to the `wire` being driven.

There are four of these rows, one per bit of `a`:

.. literalinclude:: /guide/multiplier/src/multiplier.vir
    :language: virdant
    :lines: 19-37
    :dedent:

Shifting by Hand
----------------
On paper, each partial product gets shifted one place further left.
Row `i` needs to move `i` places to the left.

How do we shift in Virdant?
There is no `<<` operator.
Instead, we reach for our old friend `word()`,
and simply glue zeros onto the *bottom* of the value:

.. code-block:: virdant

    word(row1, 0w1)    // row1, shifted left by one
    word(row2, 0w2)    // row2, shifted left by two

The literal `0w1` is a one-bit zero -- the `w1` after the 0 declares its width.
Appending `0w1` to the bottom is exactly a left-shift by one,
and we can append as wide a zero as we like.
This is also why we write `0w1` rather than `false` here:
a width-carrying zero keeps the sizes obvious at a glance.

.. note::

    If you find yourself wanting `>>` (a right-shift),
    the same trick works in reverse:
    slice off the bottom bits with `x[hi..lo]`.

Adding It Up
------------
Four partial products, each slid into position.
What remains is to add them all up.

.. literalinclude:: /guide/multiplier/src/multiplier.vir
    :language: virdant
    :lines: 39-50
    :dedent:

We sum with `+.`, the widening add from the :doc:`popcount lesson <../popcount/index>`,
and it earns its keep here.

Watch the widths grow.
`word(0w1, row0)` pads `row0` out to `Word[5]`
(the `0w1` glued on the left is just a zero),
and `word(row1, 0w1)` is also a `Word[5]`.
Their widening add produces a `Word[6]`,
which lines up perfectly with `word(row2, 0w2)`.
The next widening add produces a `Word[7]`,
which lines up with `word(row3, 0w3)`.
And the final widening add produces... `Word[8]`. Exactly `product`.

Notice how each `0wn` is exactly as wide as the shift it performs:
one, two, and three bits of padding line up with the row numbers.

Every carry gets kept, all the way to the top.
No overflow, no silent wraparound.

Why `Word[8]`?
--------------
It is worth pausing on the result type.

The largest possible product is 15 x 15 = 225,
and 225 needs eight bits: `0b11100001`.

There is a general rule hiding here:
multiplying an *n*-bit number by an *m*-bit number
always fits in *n + m* bits.
Our 4 x 4 multiplier needs 4 + 4 = 8.
If you remember this rule,
you will never have to guess the width of a product again.

.. note::

    Virdant is watching these widths for you.
    Had you declared `product` as a `Word[7]`,
    the final widening add would not fit,
    and `vir check` would refuse the module.

Cost and Speed
--------------
What did this cost us?

The four rows take 16 AND gates.
The three widening adds take a few dozen more,
depending on how the tools choose to build them.

Notice what we did *not* build: one giant adder.
Because the rows are summed a pair at a time,
no single carry has far to travel --
the same "trees beat chains" idea from the popcount lesson.

Real multipliers push this idea much harder.
Circuits like the *Wallace tree* rearrange the partial products
to shave the adders down to the absolute minimum depth.
And on modern FPGAs, you usually wouldn't build a multiplier
out of gates at all:
they ship dedicated multiply blocks (DSPs) for exactly this purpose.

.. note::

    One caveat before you ship this to production:
    this multiplier treats its inputs as *unsigned* numbers.
    Signed multiplication in two's complement is a different beast --
    the most-significant bits mean something different,
    and sign extension replaces zero extension.
    A topic for another day.

Waveform
--------
Here is `Multiplier4` taking a tour of the multiplication table:
some squares, and plenty of products where the two factors disagree:

.. vcd:: trace.vcd

    label Inputs
    signal top.a:dec
    signal top.b:dec
    space

    label Partial Products
    signal top.row0:dec
    signal top.row1:dec
    signal top.row2:dec
    signal top.row3:dec
    space

    label Product
    signal top.product:dec

The partial products dance around as the bits of `a` and `b` turn on and off.
Squint at `a` = 7 and `b` = 15:
only three of the four rows survive,
since `a` = `0b0111` keeps just three of its mask bits set --
but each surviving row is filled to the brim,
since `b` = `0b1111` sets all four of its bits.
The answer is 105.
That asymmetry is the whole idea of partial products:
`a` decides *which* copies of `b` survive;
`b` supplies the copies.

Take a moment to find a favorite input of yours in the trace --
say, `a` = 9 and `b` = 9 --
and trace how 81 assembles itself out of four rows and three adds.
Or chase down the grand finale,
15 x 15 = 225,
where every row is full and every carry climbs.

Next Steps
----------
We now have adders, counters, and multipliers --
quite an arithmetic toolkit.

Next, we will turn from arithmetic to decision-making,
with a lesson on multiplexers:
circuits that choose between values.
