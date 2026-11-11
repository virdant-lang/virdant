Pulse-Width Modulation
======================
Registers are often used to count things.
In this example, we'll use a register to count time.

`Pwm` is short for Pulse-Width Modulation.
It is a fancy way of saying that a signal is turned on and off,
but you control the ratio between the two.

Many hardware devices, such as RGB LEDs and servo motors are controlled through PWM.

.. literalinclude:: src/pwm.vir
    :caption: pwm.vir
    :language: virdant

`Pwm` has two `incoming` ports: `clock` and `duty`,
and one `outgoing` port: `out`.

Operation
---------
The output `out` will toggle on and off endlessly.
The ratio of "on" time to "off" time is called the duty cycle.


To set the duty cycle, we hold the `duty` input at the desired value.
In general, a `Word[n]` type can represent 2^n different values,
Since `duty` has the type `Word[3]`,
it can be set to any of the values `0` `1` ... `7`.

The percentage is just the value of `duty` divided by 8.
So when `duty` is `4`, `out` is on for 4/8 = 50% of the time.
When `duty` is `2`, `out` is on for 2/8 = 25% of the time.
When `duty` is `0`, `out` stays low.


Order of Statements
-------------------
The driver block for `out` looks pretty simple:

.. literalinclude:: src/pwm.vir
    :language: virdant
    :dedent:
    :lines: 5-7

`out` is set to `true` whenever `sweep` is less than `duty`.

But wait. What is `sweep`?

In Virdant, the order in which statements appear does not matter.
This gives you, the designer, a lot of flexibility
and ability to express your design the way you think works best.


Addition
--------
Thankfully, `sweep` is defined just below:

.. literalinclude:: src/pwm.vir
    :language: virdant
    :dedent:
    :lines: 9-11

Here we see our first arithmetic operator: `+` for addition.

For programmers, we often think of arithmetic as cheap.
However, in hardware, arithmetic is considered pretty expensive.
To get an idea, `a + b` will result in roughly 5 times as many gates as `a & b`.

In this case, `sweep` is a `Word[3]`,
so the expression `it + 1` lays down about 15 logic gates.
Inside the :tooltip:`ALU (Arithmetic Logic Unit)` of a 64-bit CPU,
however, the unsuspecting `+` operator would lay down around 320 logic gates!

.. note::

    Multipliers in hardware are even more expensive.
    A naive implementation of single-cycle multiplication requires around 24000 logic gates!!

In `Pwm`, the `sweep` register acts as a counter that just counts up and up forever.
Since it is a `Word[3]` it can only hold values `0` `1` ... `7`,
so when it counts up past `7`, it wraps back around to `0`.

`sweep` is used in `Pwn` to mark the time.
Whenever `sweep` is `0`, we can think of it as the beginning of an 8-cycle period.
As long as `sweep` is below `duty`, `out` is high (and otherwise, it's low).

You'll notice that in this driver, `it` appears on *both* sides of the `<=` statement.
Each clock cycle, a latched driver (that is, `<=`) takes effect.
The `it` on the right hand side (in the expression `it + 1`)
refers to the previous value of `counter`.
When it appears on the left hand side (`it <=`),
we think of this as preparing to set the *next* value for `counter`.

.. tip::

    When using continuous drivers (`:=`),
    the port on the left hand side may *never* appear on the right hand side.
    The result would be what's called a combinational loop.

    Combinational loops are considered errors in Virdant.

Uninitialized Registers
-----------------------
Finally, it is worth pointing out a subtle detail.

While the driver tells Virdant how the value of a `reg` changes at each clock tick,
it doesn't say what value it starts with.

By default, a register is uninitialized at start up.
It can take on any value.

In the :doc:`Virdant simulator </tools/run>`, registers are initialized randomly by default.

In actual hardware, it depends entirely on the hardare.
Most FPGAs support register initialization.
Many will automatically initialize to `0`, but it is bad practice to rely on this.
When designing for an :tooltip:`ASIC (Application-Specific Integrated Circuit)`,
register initialization *is never* supported.

For our simple example, because `sweep` is meant to wrap around and around,
counting from `0` to `7` indefinitely,
we chose not to initialize it.
For most applications (working with LEDs and motors),
the precise initial value won't make any difference.


Waveforms
---------
Here are a few waveforms with various values of `top.duty`.

Duty 4 -- 50%:
A perfect square wave: four cycles high, four low.

.. vcd:: trace_duty4.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 2 -- 25%:

.. vcd:: trace_duty2.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 5 -- 62.5%:

(Because 5/8 = 62.5%)

.. vcd:: trace_duty5.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 1 -- 12.5%:

A single high pulse at the start of every period.

.. vcd:: trace_duty1.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 0 -- always low:

`sweep` is never below 0, so `out` never rises.
The zero duty value is a completely flat output.

.. vcd:: trace_duty0.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out
