Counter with Wraparound
=======================
In the previous lesson we built a counter with the `mux` primitive
(:doc:`../counter_mux/index`).
One `mux` was enough, because the counter had exactly one decision to make:
either count up, or don't.

Real counters rarely get off that easy.
Here we will build `CounterWrap`, a counter which:

* counts `0` `1` `2` ... up through `9`,
* then wraps back to `0` (instead of rolling over through `10` `11` ...),
* and jumps back to `0` whenever `reset` is asserted.

That is two special conditions plus a default behavior.
You *could* express this as `mux` nested inside `mux`,
but the result gets hard to read fast.
This lesson introduces the tool built for exactly this situation:
the statement-position `if` block.

.. literalinclude:: /guide/counter_wrap/src/counter_wrap.vir
    :caption: counter_wrap.vir
    :language: virdant

An `if` Block in Statement Position
-----------------------------------
Here is the register which does all the work:

.. literalinclude:: /guide/counter_wrap/src/counter_wrap.vir
    :language: virdant
    :lines: 5-18
    :dedent:

The `if` block sits in *statement* position:
it lives inside the driver block for `count`,
and each of its arms is itself a little driver block.
That is the difference from the `if` blocks of the multiplexer lesson,
where each arm produced a *value* after a `=>`.
Here, each arm *performs* writes inside `{ ... }`.
(The rule of thumb: `{` performs, `=>` produces.)

.. note::

    In Verilog, this register would be an `always` block
    with `if` / `else if` / `else` statements inside.
    Virdant's statement-position `if` fills that same role,
    but the compiler knows which register each arm is driving,
    so there is no sensitivity-list to get wrong.

`when` Guards
-------------
Each `when` clause is a **guard**: a condition of type `Bit`
followed by the driver block to run when that condition holds.

.. literalinclude:: /guide/counter_wrap/src/counter_wrap.vir
    :language: virdant
    :lines: 8-13
    :dedent:

The guards are tested **in order**, and the first one which holds fires.
This is exactly the priority-mux behavior from the multiplexer lesson,
expressed in statement form instead of expression form.

Here the order matters:
`when reset` comes first, so reset wins over everything else.
Even if `count` is sitting at `9` when `reset` goes high
(so that both guards hold at once),
the reset arm is the one which runs.
Put the most urgent condition first.

`else`
------
The `else` arm is the fall-through: it runs when *no* `when` guard held.

.. literalinclude:: /guide/counter_wrap/src/counter_wrap.vir
    :language: virdant
    :lines: 14-16
    :dedent:

This is the ordinary counting step: on every clock edge which is neither
a reset nor the wrap point, `count` becomes `count + 1`.

The `else` arm is optional here.
If no arm fires on some clock edge, a `reg` simply keeps its old value --
registers hold their state until something writes them.
(For wires, by contrast, every path must produce a value,
so a missing case is an error.)

Reading It as a Priority Mux
----------------------------
It is worth spelling out what hardware this describes.
Each clock edge, the next value of `count` is chosen by a little priority mux:

=========  ======  =====================
Condition  First?  Next value of `count`
=========  ======  =====================
`reset`    yes     `0`
`it == 9`  no      `0`
otherwise  --      `it + 1`
=========  ======  =====================

The `if` block is just a friendlier way to write that mux,
one arm at a time, in the order the conditions are checked.

Compared to the `mux`-Based Counter
-----------------------------------
The counter from the previous lesson drove its register
with one `mux` expression per decision.
To get this same wrap-and-reset behavior with `mux` alone,
we would have to nest them:

.. code-block:: virdant

    reg count : Word[4] {
        initial it <= 0
        it <= mux(reset, 0, mux(it == 9, 0, it + 1))
    }

It works -- a chain of `mux`es *is* a priority mux -- but the reader has to
untangle the nesting to see the three cases.
The statement-position `if` lays the same logic out flat,
one condition per arm, in priority order.

.. note::

    The two forms are two spellings of the same hardware:
    both lower to a mux choosing the register's next value.
    Choose the expression form (`=>`) when you are driving one target
    with a simple choice, and the statement form (`{ ... }`) when the
    conditions select between *actions* -- especially when several
    registers update together under the same conditions.

Trying It Out
-------------
The demo script `counter_wrap.poke` counts all the way around the wrap
and pokes `reset` in mid-count to check both behaviors:

.. code-block:: none

    $ vir poke counter_wrap.poke --script
    top: counter_wrap::CounterWrap
    top.reset <= true
    top.reset <= false
    top.reset <= true
    top.reset <= false

Every `assert` passes: the counter walks `0` through `9`, wraps back to `0`,
holds at `0` while `reset` is asserted, and jumps back to `0`
when `reset` arrives mid-count.
