Full Adder
==========
Our `HalfAdder` is good enough to add any pair of 1-bit numbers together.
However, if we want to sum up numbers with more than 1 bit,
we need just a little bit more.

A `FullAdder` is a famous circuit which can sum two 1-bit numbers *and* a carry.
(This is a total of three 1-bit numbers).
It is made up of two `HalfAdder`\s, thus the name:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :lines: 16-39

.. note::

    This is the second in a series of three modules which appear in full in :doc:`../adder/index`.

In addition to `a` and `b`, `FullAdder` takes an additional 1-bit input called `carry_in`.
The circuit then adds `a`, `b`, and `carry_in` together and returns their sum as `carry_out` and `sum`.


The First Half Adder
--------------------
The first `HalfAdder` is this:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :dedent:
    :lines: 28-32

The `mod` keyword, when used inside a module definition,
creates an *instance* of another module.
Here, we give the submodule the name `halfadder_0`
and using the `of` keyword, we say it is going to be a `HalfAdder`.

We create a driver block to connect up the ports of `halfadder_0`.
We write `it.a := a`, and similarly `it.b := b`,
to thread the `incoming` port values `a` and `b`
down to our first half adder.

Once we have wired things up this way,
`halfadder_0` will sum `a` and `b` for us.
The sum of `a` and `b` are made available through its `outgoing` ports,
which we may refer to as `halfadder_0.sum` and `halfadder_0.carry_out`.


On Naming Things
----------------
There is a certain kind of designer who is always tempted to use
very short names for signals and submodules.
Perhaps they might want to write `ha` instead of `halfadder`.

But in Virdant, we use highly descriptive and memorable names,
even when they are a bit longer.
This is one way you can be courteous to the designer who has to read your code.

Part of the reason we have `it` in Virdant
is so you don't have to type the names out more than once or twice.
So always think carefully about your variable names,
and don't skimp on letters.

When naming things that have numbers,
it is conventional to start numbering starting at `0`
and to always place an underscore before the number.
That's why our "first" half adder is not `halfadder_1`,
nor is it `halfadder0`,
but rather it is `halfadder_0`.


Second Half Adder
-----------------
Our second `HalfAdder` submodule is this:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :dedent:
    :lines: 34-38

The difference here is that we do not sum `a` and `b`,
but rather `halfadder_0.sum` and `carry_in`.
Since `halfadder_0.sum` gives us the value `a` + `b`,
this has the effect of summing `a` + `b` + `carry_in`,
just as we wanted.


Wrapping up the Full Adder
--------------------------
The two remaining parts of `FullAdder` are a few lines we skipped over
which drive the outputs `sum` and `carry_out`.

These lines should be straightforward,
as long as you remember `||` is how we say logical OR in Virdant:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :dedent:
    :lines: 21-26


Waveform
--------
The waveform for this example is much larger:

.. vcd:: ../adder/fulladder.vcd

    label inputs
    signal top.a
    signal top.b
    signal top.carry_in

    label outputs
    signal top.sum
    signal top.carry_out

    space
    label First Half Adder
    signal top.halfadder_0.a
    signal top.halfadder_0.b
    signal top.halfadder_0.sum
    signal top.halfadder_0.carry_out

    space
    label Second Half Adder
    signal top.halfadder_1.a
    signal top.halfadder_1.b
    signal top.halfadder_1.sum
    signal top.halfadder_1.carry_out

It's larger because we are showing not only the ports for `FullAdder`,
but also the ports for its two submodules, `halfadder_0` and `halfadder_1`.

Note that because of the way the signals are driven,
many of these signals are aliases for one another.
For example, `top.a` and `halfadder_0.a` are copies of the same signal.
This is because we have a driver `it.a <= a` (where `it` refers to `halfadder_0`).

If we were only interested in the behavior of the top-level ports,
we could trim it down:

.. vcd:: ../adder/fulladder.vcd

    label Inputs
    signal top.a
    signal top.b
    signal top.carry_in

    label Outputs
    signal top.sum
    signal top.carry_out

Or if we were trying to understand a bug in a design,
we might look at a few key ports from different levels of
the module hierarchy:

.. vcd:: ../adder/fulladder.vcd

    label Inputs
    signal top.a
    signal top.b
    signal top.carry_in

    label First Half Adder
    signal top.halfadder_0.a
    signal top.halfadder_0.b
    signal top.halfadder_0.sum
    signal top.halfadder_0.carry_out

This should give you an idea of how there is a bit of an art
to how to think about and visualize what a module does.

We didn't go into as much detail over how `FullAdder` works this time,
so make sure to study the code and the waveforms carefully!

It may look really complicated.
Don't worry too much if you don't get it at first.
It's enough to understand that we're just adding three 1-bit numbers together.
And we're doing so with logic gates and
by re-using the logic inside of `HalfAdder` from the last lesson.
