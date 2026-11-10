Shift Register
==============
.. literalinclude:: src/shiftreg.vir
    :caption: shiftreg.vir
    :language: virdant

- `reg`
- `match` with `Valid`/`Invalid`
- `word()` and slices for shifting
- `shift_out` from a single bit of a register

Waveform
--------
Here is the shift register in action:
loaded with a byte, then clocked out one bit at a time
while arbitrary bits wander in through `shift_in`.

.. vcd:: trace.vcd

    signal top.clock:clk
    signal top.load_value:Valid[Word[4]]
    signal top.shift_in
    signal top.value:bin
    signal top.shift_out

Notice two things.
First, the `load_value` handshake:
decoded as a `Valid[Word[8]]`, for exactly one cycle it carries `@Valid(179w8)`,
and the register takes the whole byte at once;
after that it goes `@Invalid` and the register shifts instead.
Second, watch `shift_out` read out the original byte MSB-first —
`1, 0, 1, 1, 0, 0, 1, 1` —
even as `shift_in` feeds unrelated bits into the bottom of the register.
That is serial transmission in a nutshell:
eight flip-flops, and one bit leaves per clock.
