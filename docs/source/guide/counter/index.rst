Counter
=======
.. literalinclude:: src/counter.vir
    :caption: counter.vir
    :language: virdant

- `if`
- resets
- `+`
- `==`
- `initial`
- overflow

Waveform
--------
Here is the counter in action:
held in reset, counted up through the wrap, paused, run backwards
through the wrap, held while both inputs assert, then released.

.. vcd:: trace.vcd

    signal top.clock:clk
    signal top.reset
    signal top.increment
    signal top.decrement
    signal top.counter:dec

Notice three things.
First, before the first `reset`, the counter starts in an unknown state:
without a reset, a register holds *some* value, but not one you chose.
Second, holding both `increment` and `decrement` low pauses the count in place,
and when `increment` comes back, the count resumes right where it left off.
Third, holding both inputs high at once is a hold too:
no guard (`increment && !decrement` or `decrement && !increment`) fires,
so the register keeps its old value.
