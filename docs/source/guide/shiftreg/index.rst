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

On the other hand, we can load a new value by supplying `load_value`,
replacing the old value of the register.

At any point, we can read the value of the register with `value`.


The `Valid` Type
----------------
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


Matching on `Valid`
-------------------
Here is the heart of the module:

.. literalinclude:: src/shiftreg.vir
    :language: virdant
    :lines: 9-22
    :dedent:

This is a bit heavy to take in at a glance,
so let's break it down.

We see this `match` statement.

.. code-block:: virdant

    match load_value {
        with @Valid(new_value) {
            it <= new_value
        }
        with @Invalid() // ...
    }


The first pattern is `with @Valid(new_value)`.
This matches whenever `load_value` is a valid value.
When it matches, it also *binds* the payload to the name `new_value`.

.. tip::

    This value is only accessible from within the block.
    This means you can't accidentally use it in a place later on when the value isn't actually valid.
    (This means less bugs!)

The second pattern is `with @Invalid()`.
It matches when there is no value to load.


Chained Conditionals
--------------------
Occasionally, the arm of a `match` (or `if`) statement (or expression)
naturally leads to immediately performing *another* `match` (or `if`):

.. code-block:: virdant

    with @Invalid() {
        match shift_in {
            with @Valid(new_msb) {
                it <= word(new_msb, it[4..1])
            }
        }
    }

Virdant allows us to write the above block like this instead (which we do in our example):

.. literalinclude:: src/shiftreg.vir
    :language: virdant
    :lines: 14-18
    :dedent:

This syntax is called condition chaining.
It lets us save a pair of parentheses.
It also avoids a level of indendation.

In general, we should always write nested conditionals this simplified form
whenever we have the opportunity.


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
