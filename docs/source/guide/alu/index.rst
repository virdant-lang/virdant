ALU
===
In this example, we'll build an ALU --
an **arithmetic logic unit**, the part of a processor which
performs addition, comparison, and bitwise logic.

Ours will be modeled after the classic RISC-V ALU:
instead of a single opaque opcode, the caller supplies the `funct3` and
`funct7` fields straight out of a RISC-V instruction,
and the ALU answers with a 32-bit `result`.

In the :doc:`previous chapter <../shifter/index>` we built a barrel
shifter, met our first **enum type**, and caught a glimpse of
**match expressions**.
Now both come into their own:
an ALU turns out to be the perfect excuse for them.

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant


Enum Types
----------
A RISC-V instruction spends a 3-bit field, `funct3`, saying *what kind*
of register-register operation it is.
Every one of the eight possible values has a name,
so this is a natural job for an **enum type**:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 3-13

An enum type maps named values onto concrete bit patterns.
Here, `#Add` *is* `0b000`, `#Xor` *is* `0b100`, and so on.
(Normally the ``#`` prefix looks odd, but think of it as
"give me the variant, not the variable".)

This is the other flavor of enum declaration.
The shifter's `ShiftDirection` wrote out **no** values,
and Virdant one-hot encoded them for us.
Here we write out **all** of them,
because the bit patterns are not ours to choose --
they come straight from the RISC-V encoding,
and the first literal, `0b000w3`, carries an explicit width
that pins the entire enum type at exactly 3 bits wide.
Either way, Virdant insists you pick a lane:
all the values, or none of them.

Notice also that the enum type covers **all eight** `funct3` codepoints --
including `#Sltu`, an operation our ALU is not going to implement.
Keeping the full RISC-V field in the type, holes and all,
turns out to pay off later.


Funct7
------
Sharp-eyed RISC-V readers know a secret about `funct3`:
there is no codepoint for subtraction!
Both ADD and SUB claim `funct3 = 0b000`,
and the *seventh* bit of `funct7` (`0b0100000` for SUB, `0b0000000` for ADD)
tells them apart.

Rather than invent our own merged opcode, we accept the fields as they are:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 15-19

Two operands, a `funct3` enum type, and a raw 7-bit `funct7`.
Clarity beats cleverness.

(`funct7[5]` will pick up a second job before this chapter is over:
telling SRL and SRA apart, which share `funct3 = 0b101`
the same way ADD and SUB share `funct3 = 0b000`.)


``match``
---------
Now, the centerpiece.
Given one of eight possible `funct3` values, we need to select one of
eight possible computations.
Virdant's tool for this is the **match expression**:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 35-53

A match expression inspects a value (here, `funct3`) and picks an arm.
Each arm begins with the `with` keyword, a pattern to compare against,
and `=>`, followed by the value the arm produces.

The patterns are the enum variants, written with the same ``#`` prefix:
`with #Add => ...`, `with #And => ...`.
Since we're inside a driver block for `result`,
`it` on the left of `:=` refers to `result`,
and the whole `match` produces exactly the value `result` should take.

Two properties make `match` special:

* **Exhaustiveness.**
  Virdant will refuse to compile unless every enumerant of `Funct3`
  is covered by some arm.
  Forget `#And`, and the compiler tells you so, by name.
  Add a ninth variant to the enum someday, and every `match` on it
  sprouts a helpful error marking exactly where you forgot.
* **Parallelism.**
  A `match` is not a chain of tests, one after another.
  All arms are examined *at once*;
  the hardware is a single mux tree, not a priority encoder.
  However many enumerants your enum grows to,
  a `match` costs the same latency.


ADD and SUB
-----------
Most arms are one-liners, but the `#Add` arm has a surprise inside:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 41-44

`funct3` said "add", but `funct7` gets the final word:
if it reads `0b0100000`, this is really a subtraction.

Since this arm needs a *condition*, we reach for Virdant's other
conditional structure, `if`, written here in its expression form:
conditions after `when`, values after `=>`,
and -- because every expression must have a value -- a mandatory `else`.

.. note::

    The distinction is worth memorizing:
    in *statement* position (like the `reg` updates in earlier examples),
    conditional arms are driver blocks wrapped in ``{ ... }``.
    In *expression* position, arms are values after ``=>``.
    Mixing them up is a compile error, not a mystery.


Sign Comparison
---------------
The `#Slt` arm (set less-than) compares its operands as **signed**
integers and answers `1` or `0`.
But ``<`` in Virdant compares words the straightforward unsigned way,
so we do a little two's-complement reasoning ourselves:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 45-48

Recall that a 32-bit two's-complement number is negative exactly when
bit 31 is set.
If the two operands *disagree* about sign, the comparison is already
decided: the result is just the sign bit of `operand_a`.
Otherwise, both are on the same side of zero, and unsigned `<` gets it right.
The `mux(condition, then, else)` builtin selects between the two cases,
and `zext` widens a `Bit` into a full `Word[32]`.


Shifting
--------
That leaves the shifts --
and here the ALU gets to be lazy, because we already did the work.
The :doc:`previous chapter <../shifter/index>` built a 32-bit barrel
shifter that handles SLL, SRL, and SRA in a single cycle,
so this chapter's project simply carries it along as a second package:

.. code-block:: virdant

    import shifter

An `import` brings another **package** into scope.
A package is one `.vir` file, named after the file it lives in:
the `Shifter` module lives in `shifter.vir`,
sitting right next to `alu.vir` in this chapter's `src/` directory.

With the package imported, we instantiate the shifter as a submodule
and wire it up:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 21-33

`mod shifter of Shifter` creates an instance of the `Shifter` module,
and the driver block drives its incoming ports by name:
`it.direction`, `it.value`, `it.amount`.

The direction wiring is where `funct7` does its second job.
RISC-V reuses `funct3 = 0b101` for both SRL and SRA,
with `funct7[5]` as the tiebreaker -- the same trick ADD and SUB play
with `funct3 = 0b000`.
The guarded arm `with #Srl if { ... }` is the same if-expression form
the `#Add` arm uses, just producing enum variants instead of sums.

Meanwhile `operand_b[5..0]` is RISC-V's 5-bit `shamt` field --
which is *why* the shifter's `amount` is exactly `Word[5]`:
the instruction encoding only ever delivers 0 through 31.

Finally, both shift arms of the big `match` forward the shifter's answer:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 50-51

Notice that the ALU's `match` never mentions arithmetic at all.
Whether SRL means *logical* or *arithmetic* was already folded into
`it.direction`, and the shifter took it from there.
That's the payoff of building the shifter as its own module:
the ALU doesn't need to know how shifting works,
only that it does.


Saying "Don't Care"
-------------------
One enumerant remains, and we handle it dishonestly, on purpose:

.. literalinclude:: src/alu.vir
    :caption: alu.vir
    :language: virdant
    :dedent:
    :lines: 49

`dontcare` means *this value is genuinely unspecified*.
Maybe nobody downstream will ever read it; maybe any answer will do.
Virdant hears you, stays silent, and quietly hands the optimizer
permission to put whatever is cheapest there.
In simulation you may observe some fixed value (ours happened to be zero),
but nothing anywhere promises it will stay.

.. note::

    Virdant has a second escape hatch, the ``?`` **hole**,
    for logic you haven't written *yet* --
    a hole typechecks as whatever the context needs,
    and the compiler tags it with a warning until you fill it in.
    This ALU doesn't need one:
    every remaining operation now has a real answer.


Trying It Out
-------------
Let's take the ALU for a spin with ``vir poke``,
Virdant's interactive peek-and-poke tool.
Enum variants poke in directly as ``#Name``:

.. code-block:: text

    $ vir poke
    vir poke: top alu::Alu (type `exit` to leave, `help` for commands)
    top> poke funct3 #Add
    top> poke funct7 0b0000000
    top> poke operand_a 0x45
    top> poke operand_b 0x2a
    top> print result
    0b00000000000000000000000001101111

`0x45` + `0x2a` = `0x6f`, in binary, 32 bits wide.
Now the funct7 trick:

.. code-block:: text

    top> poke funct7 0b0100000
    top> print result
    0b00000000000000000000000000011011

Same `funct3`, opposite operation: `0x45` - `0x2a` = `0x1b`.
The bitwise ops need no new inputs at all,
so flipping through them is quick:

.. code-block:: text

    top> poke funct3 #And
    top> assert result == 0x0f000f00
    top> poke funct3 #Or
    top> assert result == 0xff0fff0f
    top> poke funct3 #Xor
    top> assert result == 0xf00ff00f

(`assert` complains loudly when its condition is false
and says nothing at all when it is true.)

Finally, signed comparison -- `0x7` versus `0xfffffffa`, which is `-6`
once you treat bit 31 as a sign:

.. code-block:: text

    top> poke funct3 #Slt
    top> poke operand_a 0x00000007
    top> poke operand_b 0xfffffffa
    top> assert result == 0x00000000
    top> poke operand_a 0xfffffffa
    top> poke operand_b 0x00000007
    top> assert result == 0x00000001

Seven less than negative six?  Certainly not.
Negative six less than seven?  Of course.

And the shifts, courtesy of last chapter's barrel shifter.
First a logical right shift of `0xdeadbeef` by 4:

.. code-block:: text

    top> poke funct3 #Srl
    top> poke funct7 0b0000000
    top> poke operand_a 0xdeadbeef
    top> poke operand_b 0x00000004
    top> assert result == 0x0deadbee

Now flip `funct7[5]` -- same `funct3`, same operands --
and the vacated bits fill with sign bits instead of zeros:

.. code-block:: text

    top> poke funct7 0b0100000
    top> assert result == 0xfdeadbee

Try it on `0x80000000` shifted right by 31 if you want to see
the sign extension in its full glory: `0xffffffff`.


Where To Next
-------------
The ALU has come a long way from a pile of holes,
and there are still honest ways to grow it:

* Give `#Sltu` its true meaning -- one borrow bit away, thanks to the `-.`
  operator's widen-and-borrow form.
* Or hook this ALU up to a real RISC-V decoder, which now only has to
  slice fields out of an instruction and feed them in by name.
