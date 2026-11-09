Synchronizing a Button
======================
So far, every input our circuits have seen arrived already in step with
our clock.
A real board is not so polite.

Press a pushbutton on an FPGA board and the signal travels in from a pin
whenever the finger decides, mid-cycle, bouncing on its way.
If we feed that raw pin straight into a register,
we are sampling a signal that may be *changing at the exact moment*
we sample it.

When that happens, the register can enter *metastability*:
caught between a 0 and a 1, its output hovers at some in-between voltage
for an unknowable length of time before toppling one way or the other.
A downstream register that reads that wobbling output
may see garbage, or disagree with its neighbor about what happened.
You cannot eliminate the risk -- physics does not allow it --
but you can shrink the window until it is negligible in practice.

The Two-Flop Fix
----------------
The standard cure is to pass the pin through two registers in a row,
both clocked by our own clock:

.. code-block:: virdant

    reg meta : Bit {
        it <= inp
    }

    outgoing reg out : Bit {
        it <= meta
    }

The first flop is the one that takes the risk:
it may go metastable when it samples the changing pin.
But it has a full clock cycle to settle before anything looks at it,
and a metastable flop almost always resolves within one cycle.
The second flop reads a calm, settled value,
and hands the rest of the design a clean signal
that changes only on clock edges.

The price is small: the synchronized copy of the input
lags the raw pin by two clock cycles.
For a human finger, two cycles at a megahertz and up is nothing.

Virdant ships this as a builtin module, `Sync`,
in the builtin package every design gets for free --
so we do not even have to write the two flops ourselves.

Instantiating `Sync`
--------------------
Here is the whole lesson: a button that toggles an LED.
We start by synchronizing the pin:

.. literalinclude:: /guide/led_button/src/led_button.vir
    :language: virdant
    :lines: 6-11
    :dedent:

The line `mod sync_button of Sync` creates an *instance*:
a copy of the `Sync` block's hardware, named `sync_button`.
(This is the same instantiation syntax we met in :doc:`../pwm/index`.)

Its ports are connected in an `it` block,
where `it` refers to the instance:

* `it.clock := clock` tells the synchronizer which clock to settle on --
  ours, the one the rest of the design runs on.
* `it.inp := button` feeds the raw, asynchronous pin into the first flop.

From here on, the design never touches `button` again.
Everything reads `sync_button.out`,
the tamed, two-cycles-late copy of the pin.

.. note::

    In Verilog you would write the two flops by hand --
    two `always @(posedge clk)` blocks and a comment warning
    the next engineer not to "optimize away" the first one.

    Virdant's builtin package gives `Sync` a name and a type-checked
    port list, so the intent is spelled out in the design itself:
    `mod sync_button of Sync` says "this input is asynchronous"
    louder than any comment.

Detecting the Press
-------------------
Now that we have a clean `button` signal, what should a press *do*?
A press lasts many, many cycles --
we do not want the LED flickering the whole time it is held down.
We want one toggle *per press*, which means reacting to the *edge*:
the single cycle where the synchronized button goes from `false` to `true`.

To detect an edge, remember what the button was last cycle.
That is one small register:

.. literalinclude:: /guide/led_button/src/led_button.vir
    :language: virdant
    :lines: 13-17
    :dedent:

`button_prev` simply trails `sync_button.out` by one cycle.
Comparing the two tells us the direction of the change:

* `sync_button.out && !button_prev` --
  it is high now, and was low last cycle: a **rising edge**. A press.
* the opposite -- a falling edge: a release. We ignore it.

And the LED itself:

.. literalinclude:: /guide/led_button/src/led_button.vir
    :language: virdant
    :lines: 19-27
    :dedent:

`led` is an `outgoing reg`: on each clock edge,
a rising edge on the synchronized button flips it (`it <= !it`),
and otherwise the `if` block matches nothing and `led` holds still --
the "hold" behavior we met in :doc:`../counter_wrap/index`.

Written for a Board
-------------------
Notice what `LedButton` looks like from the outside:
a clock, a button pin, an LED pin.
That is not an accident -- this module is written exactly as it
would land on a board,
with `button` wired to a physical pushbutton and `led` to a physical LED.

But nothing here says *which* board.
Which pins? Which FPGA family?
Virdant answers that with `for` clauses and *platforms*:
write `mod LedButton for ice40::IceSugar` and the compiler checks your
ports against the board's actual, pinned port list,
pin by pin.
That is the subject of the next lesson, :doc:`../platform/index`.

Watching It Run
---------------
The lesson ships with a poke script, `led_button.poke`,
which presses the button twice and checks the LED answers each press
with exactly one toggle.
Run it from this directory with:

.. code-block:: console

    $ vir poke led_button.poke --script

.. literalinclude:: /guide/led_button/led_button.poke
    :caption: led_button.poke
    :language: virdant

First the script lets the synchronizer settle with the button released,
then presses: it pokes `button` high and ticks.
For two ticks nothing happens -- the press is still swimming through
the two flops -- and on the third tick the LED comes on.
The release produces only a falling edge, which the edge detector
ignores, so the LED stays lit.
A second press toggles it back off.

Every `assert` is checked by the simulator;
if any of them fails, `vir poke --script` exits nonzero.
