Pulse-Width Modulation
======================
.. literalinclude:: src/pwm.vir
    :caption: pwm.vir
    :language: virdant

- no need for initial

`out` is high while `sweep < duty`.
Since `sweep` runs 0 through 7 and wraps, each value of `duty` holds
`out` high for a fixed share of the 8-cycle period:
`duty` cycles out of 8.

Each poke script below zeroes `sweep` at power-on, pins one `duty`
value, and ticks through two full periods.
Because nothing changes mid-run, the two periods come out identical;
because `sweep` starts at 0, every trace opens at the start of a
period -- `out` rises immediately for every `duty` above 0, holds
high while `sweep` counts up to `duty`, then stays low until the
wrap.
Watch the high stretch grow as `duty` grows -- from nothing at all
at `duty` 0 up to seven of eight cycles at `duty` 7.
(In each waveform, `duty` is rendered in decimal.)

Waveforms
---------
Here are a few waveforms with various values of `duty`.

Duty 0 -- always low:

`sweep` is never below 0, so `out` never rises: the zero duty value
is a completely flat output.

.. vcd:: trace_duty0.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 1 -- 12.5%:

A single high pulse per period.

.. vcd:: trace_duty1.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 2 -- 25%:

.. vcd:: trace_duty2.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 3 -- 37.5%:

.. vcd:: trace_duty3.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 4 -- 50%:
A perfect square wave: four cycles high, four low.

.. vcd:: trace_duty4.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 5 -- 62.5%:

.. vcd:: trace_duty5.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 6 -- 75%:

.. vcd:: trace_duty6.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out

Duty 7 -- almost always high:
Out of every 8 cycles, `out` is high for 7; it dips low only for the
one cycle where `sweep` reaches 7, just before the wrap.

.. vcd:: trace_duty7.vcd

    signal top.clock:clk
    signal top.duty:dec
    signal top.out
