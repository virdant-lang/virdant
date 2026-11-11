Shift Register
==============
How many wires and how much time do you need
to send data from your computer to a peripheral device?

There are two strategies that come to mind:
You can send 1 bit at a time, or you can send all the bits at once.
The former requires as many clock cycles as bits, but only requires one wire.
The latter requires as many wires as bits, but only requires one clock cycle.

`ShiftReg` is a module that lets you convert between these two strategies.
You can shfit data in or out, one bit at a time.
Or you can load or read the entire register at once:

.. literalinclude:: src/shiftreg.vir
    :caption: shiftreg.vir
    :language: virdant


`ShiftReg` has 5 ports.
Other than the `clock`,
there is `shift_in` and `shift_out`, which are the serial ports,
and `load_value` and `value`, which are the parallel ports.

Operation
---------
It's helpful to see a diagram:

.. inline-svg:: shiftreg_internal.svg
    :align: center

When we shift a bit in through `shift_in`,
it becomes the new :tooltip:`MSB (Most Significant Bit)` of the register.
Each of the register's bits shift over one spot,
and the :tooltip:`LSB (Least Significant Bit)` is pushed out through `shift_out`.


Shifting Out
------------


The `Valid[T]` Type
-------------------
Most of the `incoming` ports we've seen so far had simple types: `Bit` or `Word[n]`.
The `shift_in` port has type `Valid[Bit]`
and `load_value` port has type `Valid[Word[4]]`.

`Valid` is a built-in type which is used to indicate that the signal may or may not be valid.
It takes two forms, depending on whether or not it is:

- If the value is valid, and has a payload of `v`, we write `@Valid(v)`
- If the value is invalid, we write `@Invalid()`

`Valid` types are commonly used on incoming ports to let the parent signal
whether or not it's actually sending data.
On cycles when it *is* sending data, it sends `@Valid(v)` (for some value `v`).
On cycles when it's *not*, it still has to send something -- so it sends `@Invalid()`.

.. note::

    Experience hardware developers will be used to the idea of using a separate `valid` port.
    Virdant's `Valid` type allows you to package this up into a single value.
    Moreover, as we'll soon see, Virdant prevents you from accidentally using an invalid value.
    That's something Verilog cannot do!


Implementation
--------------
Here is the heart of the module:

.. literalinclude:: src/shiftreg.vir
    :language: virdant
    :lines: 9-22
    :dedent:

This is a bit heavy to take in at a glance,
so let's break it down.

Shifting Out
------------
The `shift_out` port is simply always set to the :tooltip:`LSB (Least Significant Bit)` of `value`.
We do this by indexing into `value` with `value[0]`.

.. literalinclude:: src/shiftreg.vir
    :language: virdant
    :lines: 10
    :dedent:

The type of indexing into a `Word` type is a `Bit`.

.. note::

    Contrast this with slicing.
    `x[1..0]` would have type `Word[1]`.
    A `Word[1]` and a `Bit` are both 1-bit values,
    but the operations on them are different.


Matching on `Valid`
-------------------
The remaining logic combines three new ideas.
Let's tackle them one at a time, starting with the question:

> How do we look inside a `Valid[T]` to see if it's `@Valid` or `@Invalid`?

With `match`:

For the sake of clarity, let's look at a simplified version of our `match` statement:

.. code-block:: virdant

    match load_value {
        with @Valid(new_value) {
            // ...
        }
        with @Invalid() {
            // ...
        }
    }

The first pattern, `with @Valid(new_value)`,
matches whenever `load_value` is `@Valid`.
But when it matches, it also *binds* the payload (the actual valid value) to the name `new_value`.
That is, `new_value` is available for use -- but only inside this block!

The second pattern, `with @Invalid()`,
matches when there is no value to load.


Chained Conditionals
--------------------
Occasionally, the arm of a `match` (or `if`) statement (or expression)
naturally lends itself to immediately performing *another* `match` (or `if`):

.. code-block:: virdant

    match load_value {
        with @Valid(new_value) {
            // ...
        }
        with @Invalid() {
            match shift_in {
                with @Valid(new_msb) {
                    // ...
                }
            }
        }
    }

Virdant allows us to save a single pair of parentheses when we do this:

.. code-block:: virdant

    match load_value {
        with @Valid(new_value) {
            // ...
        }
        with @Invalid() match shift_in {
            with @Valid(new_msb) {
                // ...
            }
        }
    }

Doing this also saves us a level of indendation.

In general, we should always write nested conditionals this simplified form
whenever we have the opportunity.


Concatenation
-------------
Finally, to perform the shift, we need to concatenate
Instead, we build shifts out of `word()`,
which concatenates bits and words into a wider word.
Its arguments are listed from most-significant to least-significant,
and the width of the result is the sum of the widths of its arguments.

So `word(it[3..0], shift_in)` builds a `Word[4]`:

- `it[3..0]` is a `Word[3]`: bits 2, 1, and 0 of the current register,
- `shift_in` is a single `Bit`.

Reading the result from left to right:
old bit 2 lands in the new bit 3, old bit 1 in new bit 2, old bit 0 in new bit 1,
and `shift_in` in new bit 0.
In other words: shift everything up by one,
and insert the new bit at the bottom.
Old bit 3, which has nowhere to go, simply falls off the top ---
which is exactly what we want,
since that bit is what `shift_out` will emit this cycle.

A left-shift-by-one is just the simplest case of this recipe.
Shifting a `Word[8]` left by four? `word(x[4..0], 0w4)`.
A rotate? `word(it[3..0], it[4..3])`.
Without shift operators, `word()` and slices *are* the shifting story in Virdant.


Bit Indexing
------------
Finally, look at how `shift_out` is driven:

.. literalinclude:: src/shiftreg.vir
    :language: virdant
    :lines: 18-19
    :dedent:

`it[3]` is *bit indexing*: it picks out a single bit of a `Word[n]`,
by its position counted from the least-significant bit.
Bit 0 is the LSB, bit 3 is the MSB of a `Word[4]`.
The result has type `Bit`.

Don't confuse this with *bit slicing*, from the :doc:`Pop Count example <../popcount/index>`,
which picks out a *range* of bits:

.. code-block:: virdant

    it[3]      // a Bit     : just bit 3
    it[3..0]   // a Word[3] : bits 2, 1, and 0

The two notations differ in both width and type,
and Virdant's type system will catch you if you mix them up.
(In `it[3..0]` the top index is exclusive, as we saw before:
the slice starts *below* it.)

Notice that this gives us the whole shift register in four lines of code:
one multiplexer driven by a `match`,
one concatenation,
and one bit selected out to the output.
Eight of these side by side, and you have a byte-wide serial link.


Waveform
--------
Here is the shift register in action:
loaded with a nibble, then clocked out one bit at a time
while arbitrary bits wander in through `shift_in`.

.. vcd:: trace.vcd

    signal top.clock:clk
    signal top.load_value:Valid[Word[4]]
    signal top.shift_in:Valid[Bit]
    signal top.value:bin
    signal top.shift_out

Notice two things.
First, the `load_value` handshake:
for exactly one cycle it carries `@Valid(13)`,
and the register takes the whole nibble `0b1101` at once;
after that it goes `@Invalid` and the register shifts instead.
Second, watch `shift_out` read out the nibble MSB-first --- `1, 0, 1, 1` ---
even as `shift_in` feeds unrelated bits into the bottom of the register.
That is serial transmission in a nutshell:
four flip-flops, and one bit leaves per clock.
