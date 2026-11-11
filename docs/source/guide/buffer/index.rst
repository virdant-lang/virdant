Buffer
======
In this next example, we will demonstrate how to use registers, the stateful components in Virdant.

`Buffer` is a module which takes in some data and then, on the next clock cycle, sends it back out.

.. literalinclude:: /guide/buffer/src/buffer.vir
    :caption: buffer.vir
    :language: virdant

In this example, we declare four components:
`clock` `inp` `out` grouped together, and also `buffer` a bit below.
Finally, we have two drivers.

.. note::

    Notice there's no `dependson` this time.
    We're not ready to explain why quite yet,
    but we'll say it has something to do with the `<=` driver on the last line.

    When writing code yourself,
    Virdant will tell you when you need it or not!


Clocks
------
All digital circuits make use of a clock.
Oscillating high to low to high to low, over and over again,
the clock is a heartbeat for your design.

Old hardware in the 1980s often ran around 2 Mhz.
Tdoay, in a typical :tooltip:`FPGA (Field-Programmable Gate Array)`, clocks can run at anywhere from 10 MHz to 500 MHz.
Your computer's :tooltip:`CPU (Central Processing Unit)` runs even faster than that,
at anywhere from 2 Ghz to 6 Ghz!

Since clock signals are what synchronize the circuit, they get their own special type.
The `Clock` type is used for clock signals in Virdant.

.. tip::

    In Virdant, it is polite to name your clock signal `clock`.


Registers
---------
The centerpiece of our `Buffer` module is a register:

.. code-block:: virdant

    reg buffer : Word[32]

A register is a stateful component:
a tiny memory cell that can hold a value.
You can kinda think of them as the "variables" of hardware design.
This particular register holds 32 bits of data (a `Word[32]`, as we call it in Virdant).

Every `reg` must be synchronized with a clock.
Virdant sees that `clock` is the only clock available,
and so that is the one `buffer` is synchronized with.

A clock "ticks" at the very instant it transitions from low to high.
When this happens, all registers synchronized with that clock latch their new values.


Latched vs Continuous Drivers
-----------------------------
Just after `buffer` is declared, we see two drivers:

.. literalinclude:: /guide/buffer/src/buffer.vir
    :language: virdant
    :dedent:
    :lines: 8-9

You will notice that there are two different kinds of drivers: `:=` and `<=`.

We have already seen the first kind.
The `:=` driver is called a **continuous driver**.
It is used for all other components *except* for `reg`\s.
Since `out` is an `outgoing` (and not a `reg`), we use `:=`.
When we use `:=`,
the left hand side is made equal to the right hand side *continuously*.
That is, at every moment in time.

The `<=` driver is called a **latched driver**.
It is only used when the left hand side is a `reg`.
It indicates that the register will save the value (that is, "latch" it)
on the next clock tick.

Don't worry if you ever get the two mixed up.
Virdant will tell you if you get them wrong.

.. note::

    The `<=` syntax is *not* Verilog's non-block assignment.
    Virdant has no notion of blocking vs non-blocking assignment.


Statements Order
----------------
You may have noticed we wrote `out := buffer` *before* `buffer <= inp`.
The reason was simply that it was easier for us to explain things this way.

In Virdant, the order of things is a matter of style and preference.
All statements, including even the port and register definitions,
can occur in any order.


Waveform
--------
Let's look at the waveform viewer again.

.. vcd:: trace.vcd

    label Clock
    signal top.clock:clk
    label Signals
    signal top.inp:dec
    signal top.out:dec
    signal top.buffer:dec

We have a clock!
Pay close attention to the rising edge of the clock,
marked by arrows on the signal.
Remember, these are the points in time where the clock "ticks".

We also now have `top.buffer` as well.
However, if you look very closely,
you'll see that at every point in time,
`top.buffer` always seems exactly the same value that `top.out` has.
This is the effect of the continuous driver, `out := buffer`.

What is the relationship between `top.inp` and `top.buffer`?
Put another way, what is `buffer <= inp` doing for us?

If you look very carefully, you'll notice that
`top.buffer` is always the same as how `top.inp` was *one cycle earlier*.

What happens is this:
the value of `top.inp` is captured ("latched") at the instant of the clock tick.
Very soon after (we're talking a few nanoseconds),
`top.buffer` signal assumes that value.
