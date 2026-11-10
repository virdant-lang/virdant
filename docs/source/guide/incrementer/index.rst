Incrementer
===========
In this next example, we'll see a common use for `reg`\s: counting up.

`Incrementer` is a module which counts up from zero whenever it is asked to.

.. literalinclude:: src/incrementer.vir
    :caption: incrementer.vir
    :language: virdant

Initial Drivers
---------------
Most FPGAs have the ability to initialize registers when they first power on:

.. literalinclude:: src/incrementer.vir
    :language: virdant
    :dedent:
    :lines: 6

To supply the initial value, we write `initial` followed by a driver.
The driver must be `<=`, of course, since it's setting a register.
And since we're inside a driver block, we write `it` in place of `counter`.


Mux
---
.. literalinclude:: src/incrementer.vir
    :language: virdant
    :dedent:
    :lines: 7

Beneath the `initial` driver is another driver.
This is the one that determines the behavior of `counter` in the circuit.

This one has




it <= mux(increment, it + 1, it)


- `mux`
- `+`
- `initial`
- overflow

Waveform
--------

.. vcd:: trace.vcd

    signal top.clock:clk
    signal top.increment
    signal top.counter:dec
