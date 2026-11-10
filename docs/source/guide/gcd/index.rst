GCD
===
.. literalinclude:: src/gcd.vir
    :caption: gcd.vir
    :language: virdant

- `union type`
- `match`
- `mux`
- subtraction-based Euclidean algorithm
- `fire`/`valid` handshake

Waveform
--------
Here is the GCD unit computing `gcd(48, 18) = 6` and then,
without a reset, `gcd(21, 14) = 7`.

.. vcd:: trace.vcd

    signal top.clock:clk
    signal top.reset
    signal top.fire
    signal top.x:dec
    signal top.y:dec
    signal top.result:dec

Notice two things.
First, after `fire` pulses, `x` and `y` are subtracted from each other
cycle by cycle until the smaller operand reaches zero:
that is the whole Euclidean algorithm.
Second, `result` and `valid` hold their values after the computation
finishes, and a new `fire` reloads the operands directly from `Done`,
so back-to-back computations need no reset in between.
