Waveforms
=========
Let's look at the waveforms for the whole example.

.. vcd:: ../../examples/adder/trace.svg

   signal top.a:dec
   signal top.b:dec
   signal top.carry_in
   signal top.sum:dec
   signal top.carry_out

Notice there is no clock in this example.
`HalfAdder`, `FullAdder`, and `Adder4` are all *combinational* circuits:
they have no `reg`\s, and so nothing in them is synchronized to a clock.
Instead, every time an input changes, the new values ripple through the
circuit, and the outputs settle almost immediately.

We poked four different additions into the `Adder4`:

- First, `a = 3` and `b = 5`, with `carry_in` false.
  Sure enough, `sum` is `8` and `carry_out` is `false`.
- Then, `a = 9` and `b = 11`.
  Nine plus eleven is twenty, which does not fit into four bits!
  So we see `sum` equal to `14` *and* `carry_out` true,
  since `14 + 16 = 20`.
- Then, `a = 15` and `b = 15`, the largest two 4-bit numbers.
  Their sum is `30`, which appears as `sum = 14` and `carry_out` true.
- Finally, we set `carry_in` true as well.
  Now all five input bits are `1`,
  and the output shows the largest possible sum: `15` with `carry_out` true.

.. tip::

    Try working out `9 + 11 + 1` in binary by hand,
    and check that the ripple-carry through `fa0`, `fa1`, `fa2`, and `fa3`
    matches the `carry_out` of each `FullAdder` in the waveform.
