Counter with Mux
================
In :doc:`../mux/index`, we saw the `mux` primitive used to drive an
`outgoing` port.

Now we will use `mux` where it really shines:
inside a `reg` driver, to describe how a register's *next value*
is chosen.

We will build two counters:

* a free-running 4-bit counter which wraps around from 15 back to 0
* an enabled counter which only advances when `enable` is asserted,
  and which wraps at a `limit` input instead of at 15

Here is the whole example:

.. literalinclude:: /guide_dev/counter_mux/src/counter_mux.vir
    :caption: counter_mux.vir
    :language: virdant


A Free-Running Counter
----------------------
The register `free_count` counts through every 4-bit value and wraps:

.. literalinclude:: /guide_dev/counter_mux/src/counter_mux.vir
    :language: virdant
    :lines: 8-11
    :dedent:

The driver `it <= ...` gives the register its next value.
Everything to the right of `<=` is a single *expression*,
and here that expression is one call to `mux`.

Recall how `mux` works:
`mux(sel, a, b)` evaluates to `a` when `sel` is true
and to `b` when `sel` is false.

So the driver above says:

  "If `it == 15`, the next value is `0`.
  Otherwise, the next value is `it + 1`."

The condition `it == 15` reads the register's current value,
`0` and `it + 1` are the two candidate next values,
and `mux` picks between them every cycle.

The `initial it <= 0` line seeds the register for simulation,
so `free_count` starts each run at `0`.


An Enabled Counter
------------------
The register `count` advances only when told to,
and it wraps at the `limit` input rather than at 15:

.. literalinclude:: /guide_dev/counter_mux/src/counter_mux.vir
    :language: virdant
    :lines: 13-16
    :dedent:

The outer `mux` chooses between two behaviors:

* when `enable` is `true`, take the *advance* branch:
  `mux(it == limit, 0, it + 1)`
* when `enable` is `false`, take the *hold* branch: `it`

The advance branch is itself a `mux`,
just like the free-running counter's driver,
except that it compares against `limit` instead of 15.

The hold branch is simply `it`.
Remember, `it` in a driver block refers to the register being driven,
so an arm which produces `it` means "keep the current value".
When `enable` is low, the register re-latches its own value,
and time appears to stand still for `count`.

Because the advance arm is an expression too,
it can be dropped straight into the outer `mux` as one of its arguments.
That is what *nested* `mux`es are for:
each `mux` is just an expression producing a value,
so a `mux` may appear wherever a `Word[4]` value is expected —
including inside another `mux`.

The two outgoing ports are simple aliases which
let you watch both registers from outside the module:

.. literalinclude:: /guide_dev/counter_mux/src/counter_mux.vir
    :language: virdant
    :lines: 18-24
    :dedent:


One Expression, One Next Value
------------------------------
The key idea of this lesson is that `mux` is a primitive which appears
in *expression position*.
It does not "perform" an assignment or change state on its own;
it simply produces one value out of three inputs.

That is why the entire next value of a register can be written as
one expression on the right of `<=`:
Virdant evaluates the expression against the register's current value,
and the result becomes the register's value on the next clock edge.
Even the enabled counter —
which decides between holding, incrementing, and wrapping —
is a single expression, four lines long and fully typed.

This is also why the driver reads like a definition
("the next value *is* ...")
rather than like a sequence of commands.

.. note::

    In Verilog, you would likely write this as
    `if (count == LIMIT) count <= 0; else count <= count + 1;`
    inside an `always @(posedge clk)` block,
    possibly with a nested `if` for the enable.
    In Virdant, the whole next-value decision is one expression,
    which leaves no room for a missing `else`
    or a branch which forgets to assign.

Virdant has another way to express conditional logic in driver
position: `if` blocks, which *selectively drive* registers
with statement blocks instead of producing a single value.
That is the topic of the next lesson.


Running the Demo
----------------
The `counter_mux.poke` script ticks the design with and without
`enable`, using a small `limit` so the wrap is visible:

.. literalinclude:: /guide_dev/counter_mux/counter_mux.poke
    :caption: counter_mux.poke

First, three ticks with `enable` low:
`free_count` climbs `0 1 2 3` while `count` stays frozen at `0`.

Then we raise `enable` and set `limit` to `5`.
`count` climbs `0 1 2 3`, reaches `5`,
and on the next tick wraps back to `0`.
Meanwhile `free_count` keeps running toward 15
and wraps there on its own schedule.

Finally, we lower `enable` again:
`count` freezes wherever it was,
but `free_count` never stops ticking.

You can replay this yourself from the lesson directory:

.. code-block:: console

    $ vir poke counter_mux.poke --script
