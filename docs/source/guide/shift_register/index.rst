The Shift Register
==================
In :doc:`../barrelshifter/index`, we built a shifter that moves all
eight bits at once, in a single burst of combinational logic.
This lesson is about the opposite trade:
the **shift register**, which moves one bit per clock cycle
and costs almost nothing in gates.

A shift register is a short line of flip-flops, end to end.
On every clock edge, each flip-flop hands its bit to the neighbor below it,
a new bit arrives at the top, and the bit at the bottom falls off.

That simple motion is surprisingly useful:

* as a **delay line**:
  a bit that enters leaves exactly 8 cycles later,
  because every bit spends exactly 8 cycles in the register.
  Pipelining, timing alignment, and debouncing all lean on this.
* as a **serial/parallel converter**:
  feed bits in one at a time and the whole word is sitting there
  in parallel when you need it --
  that is how receivers in designs like :doc:`../spi/index`
  capture a serial stream.
  Read it the other direction and a parallel word
  becomes a stream of bits, one per cycle.

Here is the whole example:
an 8-bit serial-in/serial-out shift register with an enable,
plus a parallel window onto its contents.

.. literalinclude:: /guide/shift_register/src/shift_register.vir
    :caption: shift_register.vir
    :language: virdant

One Line of Shifting
--------------------
The register `contents` is the line of eight flip-flops,
and its entire next-value logic is one expression:

.. literalinclude:: /guide/shift_register/src/shift_register.vir
    :language: virdant
    :lines: 17-21
    :dedent:

We met `mux` inside a register driver in :doc:`../counter_mux/index`,
and it is doing the selecting here too.
`mux(enable, shifted_value, it)` means:
when `enable` is `true`, take the shifted value;
when `enable` is `false`, take `it` -- that is, re-latch the current value
and hold still.

The shifted value is `word(serial_in, it[8..1])`.
Recall that `word(...)` concatenates, most-significant first.
So this glues `serial_in` on as the new bit 7
and keeps the old bits 7 down through 1 underneath it:
`serial_in` enters at the top,
everything slides down one place,
and the old bit 0 is gone.

Slices, One More Time
---------------------
That slice deserves a second look, because it is easy to write wrong.
The bits we want to keep are 7, 6, 5, 4, 3, 2, and 1 --
seven bits.
The slice that grabs them is `it[8..1]`.

Remember the rule from the barrel-shifter lesson:
the top index of a slice is *exclusive*.
`it[8..1]` extracts bits `8 - 1` down through `1`,
which is bits 7 through 1, and its width is `8 - 1 = 7`.
Had we written `it[7..1]`, we would have gotten bits 6 through 1 --
only six bits --
and `word(serial_in, ...)` would have built a `Word[7]`
where the register needs a `Word[8]`.
The compiler would catch the width mismatch,
but the habit to build is:
*read the top index as where the slice stops, not as a bit you get.*

.. note::

    Verilog writes the same shift as `q <= {sin, q[7:1]};`.
    There the top index is *inclusive*: `q[7:1]` literally names
    the bits it keeps.
    Virdant's exclusive top means the same seven bits are spelled
    `it[8..1]` -- the indices name the boundaries of the window,
    not its contents.
    Both languages shift the same way;
    they just count differently.

.. note::

    Virdant has no `<<` or `>>` operators at all.
    A shift is always something you build:
    either `word()` plus slices, as here,
    or the staged combinational shifts of the barrel shifter
    when the amount is a runtime value.
    A shift register is the cheapest shift of all --
    one bit of progress per cycle, in exchange for waiting.

The enable could also be written as a selective driver,
`if { when enable { it <= word(serial_in, it[8..1]) } }`,
which leaves the register untouched when `enable` is low
instead of re-latching it.
The two forms describe the same hardware;
`mux` simply keeps the whole next value as one expression.

Two Ways Out
------------
The register is private to the module,
so we expose it through two `outgoing` ports:

.. literalinclude:: /guide/shift_register/src/shift_register.vir
    :language: virdant
    :lines: 23-31
    :dedent:

`parallel` hands out the whole `Word[8]` at once.
This is the serial-to-parallel half of the story:
after 8 cycles of shifting, an entire byte has arrived
and can be read as one value.

`serial_out` is just `contents[0]`,
the bit currently at the bottom of the line --
the one that will fall off on the next enabled shift.
Whatever the register holds streams out here, one bit per cycle.
(The natural extension is a parallel-load input:
`it <= mux(load, parallel_in, word(serial_in, it[8..1]))`
would move a whole word in at once whenever `load` is asserted.
We will leave that as an exercise -- the `mux` is already in place.)

Running the Demo
----------------
The `shift_register.poke` script shifts in the bit pattern 1, 0, 1, 1,
one bit per cycle, and then watches the same pattern
come back out of `serial_out`:

.. literalinclude:: /guide/shift_register/shift_register.poke
    :caption: shift_register.poke

First, four ticks with `enable` high.
Each tick, `serial_in` supplies one bit of the pattern
(1, then 0, then 1, then 1),
and each assert checks the whole `contents` word.
After the fourth tick, `contents` is `0b1101_0000`:
the pattern is fully inside, with the *first* bit shifted in
sitting deepest, at bit 4.

Then we hold `serial_in` at 0 and keep ticking.
Nothing new arrives, but the pattern keeps walking toward the bottom,
and `serial_out` emits it one bit per cycle:
1, 0, 1, 1.
Count the delay:
the 1 that was driven in before tick 1
does not reappear at `serial_out` until after tick 8.
An 8-deep line buys an 8-cycle delay:
every bit spends 8 cycles between `serial_in` and `serial_out`.
That is the delay line in action.

Finally, we drop `enable` to 0 and poke `serial_in` with a 1.
One tick later, `contents` is unchanged:
the `mux` chose the hold branch.
Raising `enable` again lets the shift resume.

You can replay this yourself from the lesson directory:

.. code-block:: console

    $ vir poke shift_register.poke --script

Between `parallel` and `serial_out`,
the same eight bits are visible both ways at once --
which is the whole point of a shift register:
it is the place where a serial world and a parallel world meet.
