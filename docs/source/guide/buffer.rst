Buffer
======
In this next example, we will demonstrate how to use registers, the stateful components in Virdant.

`Buffer` is a module which takes in some data and then, on the next clock cycle, sends it back out.

.. literalinclude:: /examples/buffer/src/buffer.vir
    :caption: buffer.vir
    :language: virdant

In this example, we declare four components: `clock` `inp` `out` grouped together, and also `buffer` a bit below.

.. note::

    We're still not ready to talk about `dependson`,
    but notice how, unlike in the last example,
    in this example, `out` does *not* have a `dependson` clause.


Clocks
------
All digital circuits make use of a clock.
In a typical :tip:`FPGA (Field-Programmable Gate Array)`, clocks can run at anywhere from 10MHz to 500MHz.

Since clock signals are what synchronize the circuit, they get their own special type.
The `Clock` type is used for clock signals in Virdant.
Unless you are working with multiple clocks, you should name your clock signal `clock`.

A clock typically oscillates, going high to low to high to low, over and over again.
It's like a heartbeat for your design.


Registers
---------
The centerpiece of our `Buffer` module is a register:

.. code-block:: virdant

    reg buffer : Word[32]

Every `reg` must be synchronized with a clock.
Virdant sees that `clock` is the only clock available,
and so that is the one `buffer` is synchronized with.

In Virdant, clocks are assumed to "tick" at the rising edge of the clock.
At the moment the clock goes from low to high,
all registers synchronized to that clock latch their new values.


Driver Blocks
--------------
Just after `buffer` is declared, we see a block containing two drivers:

.. literalinclude:: /examples/buffer/src/buffer.vir
    :language: virdant
    :dedent:
    :lines: 6-9

This is called a driver block.
This is a way of grouping our drivers to make the code easier to read.

Inside of a driver block, we may use the keyword `it` as a shorthand for referencing the component being declared.
So in our example, `it` is an alias for `buffer`.
This reduces the amount of typing we have to do.

Latched vs Continuous Drivers
-----------------------------
We also notice in our driver blocks that there are two different kinds of drivers: `<=` and `:=`.

The `<=` driver is called a **latched driver**.
It is only used when the left hand side is a `reg`.
It indicates that the value will be latched by the register on the next clock cycle.

The `:=` driver is called a **continuous driver**.
It is used for all other components *except* for `reg`.
And since `out` is an `outgoing` (and not a `reg`), we use `:=` instead of `<=`.
This syntax indicates that the value of the lefthand side is always equal to the expression on the righthand side.

.. note::

    The `<=` syntax is *not* Verilog's non-block assignment.
    Virdant has no notion of blocking vs non-blocking assignment.


Waveform
--------
Let's look at the waveform viewer again.

.. vcd:: ../examples/buffer/trace.svg
   :signals: top.clock,top.inp:dec,top.out:dec,top.buffer:dec

We have a clock this time!
Pay close attention to where the clock ticks
(transitions from low to high).

And of course, we have `top.buffer` as well.
The effect of the continuous driver, `out := it`,
(where `it` refers to `buffer`),
the values of `top.buffer` and `top.out` are always equal
at every point in time.

That said, the values of `top.inp` and `top.out` are *not* equal.
This is the effect of the latched driver, `it <= inp`,
(where `it` again refers to `buffer`).
This means whenever the clock goes from low to high,
the value of `buffer` takes on the existing value of `inp`.

.. note::

    The `incoming` port `inp` is also synchronized with the clock.
    This can make reading the waveforms a bit confusing at first.

    When we say "existing value" of `inp`,
    we look at the value immediately before the clock tick.
    This is because, in reality,
    the `clock` changes *nanoseconds before* the other signals do.


Using Driver Blocks
-------------------
Driver blocks give you a pleasant ability to express yourself in Virdant code.

The example we gave above emphasizes that `buffer` is the centerpiece of our `Buffer` module.
However, we could have written it a few different ways.

Here is another way that comes to mind:

.. code-block:: virdant
    :caption: buffer.vir

    mod Buffer {
        incoming clock : Clock
        incoming inp : Word[32]

        outgoing out : Word[32] {
            it := buffer
        }

        reg buffer : Word[32] on clock {
            it <= inp
        }
    }

This style keeps each component's driver close to where it's declared.

Or you could do it without driver blocks like we saw with the `Passthrough` example:

.. code-block:: virdant
    :caption: buffer.vir

    mod Buffer {
        incoming clock : Clock
        incoming inp : Word[32]
        outgoing out : Word[32]
        reg buffer : Word[32] on clock

        buffer <= inp
        out := buffer
    }

This style helps emphasize the components themselves,
treating their definitions as details only for those who are curious.

Can you think of any other ways to group the drivers?
Which do you like best?
