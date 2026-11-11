Half Adder
==========
In this example, we will learn about a few basic operators in Virdant.

A `HalfAdder` module is the simplest circuit which does something you might call "arithmetic".
This is a classic circuit everyone should know.

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :lines: 1-15

.. note::

    This is the first in a series of three modules which appear in full in :doc:`../adder/index`.


The `Bit` Type
--------------
`HalfAdder` takes two 1-bit inputs, `a` and `b`:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :lines: 2-3
    :dedent:

The `Bit` type is used for a 1-bit value.
Or we might say a boolean value.

The two values a `Bit` can take are `true` and `false`.


Driver Blocks
-------------
The `outgoing` ports of `HalfAdder` are written a bit differently than before:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :lines: 5-13
    :dedent:

These define `sum` and `carry_out`, both with type `Bit`.

But in both cases, there is a block directly beneath them.
This is called a **driver block**.

Inside a driver block, you can put drivers related to that component.
As a convenience, rather than spelling the name of the component over and over again,
you can just write it as `it`.

And so this is the same as if you had written

.. code-block:: virdant

    // sum is true when one of a and b is true (but not both)
    outgoing sum : Bit dependson a, b
    sum := a ^^ b

    // carry is true when a and b are both true
    outgoing carry_out : Bit dependson a, b
    carry_out := a && b

Driver blocks are nice because they allow you to tuck away the details
inside a level of indentation.
A friend who reads your code can then skim down the margin of a module definition,


Comments
--------
Before going on to how this module works,
it's worth pointing out that comments in Virdant begin with `//`
and go to the end of the line:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :lines: 5
    :dedent:

There are no multi-line comments in Virdant.


Truth Table
-----------
Back in elementary school, you learned to do addition vertically.
You learned how to add all combinations of numbers this way.

Let's do that with binary numbers.
It's much easier, since there's only four possibilities:

.. inline-svg:: halfadder_addition.svg
    :align: center

The only difference is the last one: 1 + 1 = '10'.
The number 2 is written '10' in binary,
since it has a '1' in the two's place
and a '0' in the one's place.

Representing 0 with `false` and 1 with `true`,
we can convert these number facts to a truth table for `HalfAdder`:

.. raw:: html

    <div class="halfadder-truthtable">
    <style>
      /* Scoped to .halfadder-truthtable so no other table on the site
         is affected.  Hover applies to body rows (td) only, never the
         header row (th).  !important is needed because the theme's
         zebra-stripe rules (tr:nth-child) outrank this selector's
         specificity and would otherwise hide the highlight on the
         striped rows. */
      .halfadder-truthtable table.docutils tbody tr:hover td {
        /* A soft, desaturated, dimmed highlighter yellow -- pure yellow
           (#ff0) is too harsh next to the page's low-saturation palette. */
        background-color: #e6dcb0 !important;
      }
      [data-theme="dark"] .halfadder-truthtable table.docutils tbody tr:hover td {
        /* Darker, dimmer version for the dark theme: keeps the same
           muted-gold hue but at a low luminance so it doesn't glare
           against the dark background.  The text switches to a light
           warm tone, since the dark #333 used on the light-theme yellow
           would be invisible on this dark fill. */
        background-color: #4a4330 !important;
        color: #e8e0c8 !important;
      }
    </style>
    <table class="docutils" align="center">
      <thead>
        <tr>
          <th><span class="code vir-inline"><span class="nv">a</span></span></th>
          <th><span class="code vir-inline"><span class="nv">b</span></span></th>
          <th><span class="code vir-inline"><span class="nv">carry_out</span></span></th>
          <th><span class="code vir-inline"><span class="nv">sum</span></span></th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
        </tr>
        <tr>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
          <td><span class="code vir-inline"><span class="kc">true</span></span></td>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
          <td><span class="code vir-inline"><span class="kc">true</span></span></td>
        </tr>
        <tr>
          <td><span class="code vir-inline"><span class="kc">true</span></span></td>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
          <td><span class="code vir-inline"><span class="kc">true</span></span></td>
        </tr>
        <tr>
          <td><span class="code vir-inline"><span class="kc">true</span></span></td>
          <td><span class="code vir-inline"><span class="kc">true</span></span></td>
          <td><span class="code vir-inline"><span class="kc">true</span></span></td>
          <td><span class="code vir-inline"><span class="kc">false</span></span></td>
        </tr>
      </tbody>
    </table>
    </div>


Logical Operations
------------------
If you look carefully at the truth table,
`carry_out` is only `true` for the last row,
when when *both* `a` and `b` are true.
Therefore, `carry_out` is the logical AND of `a` and `b`,
which we write as `a && b` in Virdant:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :lines: 11-13
    :dedent:

Again, looking *very* carefully at the table,
we see `sum` is only `true` when `a` and `b` are opposites of each other.
Therefore, `sum` is the logical XOR of `a` and `b`.
We write this as `a ^^ b` (which is a bit unique) in Virdant:

.. literalinclude:: /guide_dev/adder/src/adder.vir
    :language: virdant
    :lines: 5-9
    :dedent:


.. note::

    The other logical operations in Virdant are `a || b` for logical OR
    and `!a` for logical NOT.


Waveform
--------
The final waveform of `HalfAdder` looks like this:

.. vcd:: ../adder/halfadder.vcd

    label Inputs
    signal top.a
    signal top.b

    label Outputs
    signal top.sum
    signal top.carry_out

Remember that both `top.a` and `top.b` are inputs.
All four possible combinations of inputs are represented here.
Squint and try to "find" the addition table hidden among the squiggles.
