ALU
===
In this example, we'll build an Arithmetic Logic Unit or ALU.

`Alu` is a minimal, authentic, and slightly incomplete implementation of a RISC-V ALU.

Again, our example is a bit long,
so we'll first look at the pieces and present the full thing :ref:`at the end<full-example>`.

Packages and Imports
--------------------
A common operation most computers provide is bit shifting.
But we already did that in the last example!

Rather than copy it into our ALU example, we will keep it in its own file, `shifter.vir`.
Each `.vir` file is its own package.
You can import it like this:

.. literalinclude:: src/alu.vir
    :language: virdant
    :lines: 1

When you import a package, you can refer to its modules, type definitions, and other things inside.

You've probably noticed that
nearly everything we've defined so far in the guide has been marked as `pub`.
`pub` says "You can import me and use me!"

In :doc:`shifter <../shifter/index>`, `Shifter` was marked `pub`, so we can use it in our ALU.
`ShiftDirection` was also `pub`, so we can use it too.

However, not every module really wants to be `pub`.
If you were paying really close attention,
you might have noticed that
`ShiftLeft` `ShiftLogicalRight` and `ShiftArithmeticRight` were *not* marked as `pub`.
These modules are meant to be helpers,
so we don't really want anyone outside using them.

When learning a Virdant codebase,
you should pay attention to modules and types that are `pub`.

Enums with Values
-----------------
RISC-V supports many arithmetic operations.
Let's look at all the opcodes we will support in our ALU:

.. literalinclude:: src/alu.vir
    :language: virdant
    :lines: 3-12

Just like `ShiftDirection` in the previous example,
`Funct3` (which means the 3-bit function opcode) is an `enum type`.
But unlike `ShiftDirection`, each enumerant of `Funct3` is given a numeric value.

.. note::

    The values of `Funct3` are pulled directly from Chapter 36 of
    the `RISC-V specification <https://docs.riscv.org/reference/isa/_attachments/riscv-unprivileged.pdf>`_.

We've chosen to give the numeric values in binary here
because then it's easy to match it up with the spec.

When giving numeric values to an enum type,
at least one of the fields must have an explicit width.
Here, the first enumerant, `#Add` is given the value `0b000w3`
(remember this means `0` with width `3`).

The ALU
-------
The actual ALU is pretty straightforward.
It has four `incoming` ports:
`funct3` and `funct7` which determine the operation,
`operand_a` and `operand_b` tell you the operands.
The `outgoing` port `result` at the bottom gives you the result:

.. literalinclude:: src/alu.vir
    :language: virdant
    :lines: 14-56

Instantiating the Shifter
-------------------------
In the body, we instantiate the `Shifter` with this:

.. literalinclude:: src/alu.vir
    :language: virdant
    :lines: 20-31
    :dedent:

Here, we have to refer to it as `shifter::Shifter`, since it lives in the `shifter` package.

The three `incoming` ports of `Shifter` are driven.
`direction` uses a `match` expression.

It uses a chained `if` for the `Srlsra` case.
The reason is that `srl` and `sra` have the same opcode.
(The are both right shifts, after all).
In this case (and again with `add` and `sub`),
we use bit 5 of `funct7` to distinguish them.

.. tip::

    In fact, bit 5 of `funct7` is the only bit we use in the base RV32I instruction set.

The `dontcare` Value
--------------------
The only novelty here is the `else` branch of the `match` expression:

.. literalinclude:: src/alu.vir
    :language: virdant
    :lines: 27
    :dedent:

Here, we see `dontcare`.

`dontcare` is a special value that means you really don't care what value goes there.
Either that case is impossible, or else you don't plan on using the value.

Because the shifter is instantiated in the hardware,
it will *always* perform a shift, whether we want one or not.
But if our instruction is an `#Add` instruction,
what kind of shift do we choose to do?

Rather than deciding, `dontcare` lets us opt out of making a decision.
This serves two purposes:
First, other designers will be able to see
that you *intentionally* decided you didn't care about that case.
Second, Virdant reserves the right to choose a value that allows for more aggressie optimizations.

.. warning::

    Making use of a `dontcare` value is basically the same
    as using the value of an uninitialized `reg`.
    Don't do it!


.. _full-example:

Full Example
------------
.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant

Waveforms
---------
.. vcd:: trace.vcd

    label Operation
    signal top.funct3:alu::Funct3
    signal top.funct7:bin

    label Operands
    signal top.operand_a:dec
    signal top.operand_b:dec

    label Shifter
    signal top.shifter.direction:shifter::ShiftDirection
    signal top.shifter.amount:dec

    label Result
    signal top.result:dec
