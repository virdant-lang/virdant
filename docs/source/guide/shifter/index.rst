Shifter
=======
This next example is long, but easy.

`Shifter` is a barrel shifter:
a circuit that performs bit shifting:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 7-35

Here we have our shifter module.
It has three `incoming` ports: `direction` `value` and `amount`,
and one `outgoing` port: `result`.

Operation
---------
The operation of `Shifter` is pretty simple:
You specify `value` and `amount`,
and say which `direction` to shift,
and then `result` is the result.

But already, we see something weird:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 8
    :dedent:

What is `ShiftDirection`?

Enum Types
----------
Virdant supports fancy types.

`ShiftDirection` is a type *we* defined so that we could refer to the direction symbolically,
rather than encoding it as a number:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 1-6

We see that `ShiftDirection` is an `enum type`.
The three values of type `ShiftDirection` are the ones listed,
but we write them as `#LeftLogical` `#RightLogical` and `#RightArithmetic`.

.. note::

    For those with experience in hardware, you may be wondering:
    What *numeric* value do they have?

    In this case, we don't really care.
    And so, we did not give the enumerants explicit values.
    Virdant will choose values for us.


Logical vs Arithmetic
---------------------
Programmers likely know that `<<` and `>>` are how you do shifts in most languages.
But do you know the difference between a logical and an arithmetic right shift?
And which one is `>>`?

When shifting left by any amount, it makes good sense to simply fill the vacuum with zeroes:
`0b0000_0001 << 3` most naturally wants to be defined as `0b0000_1000`.

But when shifting right, there's a good argument for filling with zeroes:
`0b1000_00000 >> 3 = 0b0001_01000`.

But there's also an equally good argument for filling it with...
whatever the top bit is:
`0b1000_00000 >> 3 = 0b1111_01000`.

The reason is left shifting acts like multiplication by a power of 2,
and so we want right shifting to act like division by 2.
For unsigned numbers, we use a right left logical shift.
But for signed numbers, we use a right arithmetic shift instead,
filling the vacuum with the sign bit.

So which one is the `>>` operator?
It depends on the langauge.
In Python and Java, `>>` is always arithmetic.
In C, `>>` can be arithmetic or logical depending on
whether the left hand side is signed or unsigned.

Three Submodules
----------------
Next we declare three submodule:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 12-25
    :dedent:

There isn't much to say here.
It's worth noting that, no matter what `direction` we choose,
the hardware always calculates *all three* results.
It does so in parallel, so it doesn't take any longer than if we just calculate the one we want.

Take note, though!
In a lot of hardware design,
one would be tempte to write the logic for all three of these modules directly inside of `Shifter`.
In Virdant, it is considered polite to split functionality up into submodules.

Submodule are a convenient fiction.
When our design gets synthesized, the boundaries of the modules go away.
And so there is no cost to splitting your design up into as many submodules as you like.

This results in smaller module definitions,
which make the code easier to read and easier to understand.

Matching on Enums
-----------------
Our `result` is determined by this `match` expression inside of a driver block:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 27-34
    :dedent:

This should be pretty self-explanatory, hopefully.
While each submodule calculates one of the three shift results,
this `match` expression is the thing that looks at `direction` to select between them.


Shifting Left
-------------
Let's go over the `ShiftLeft` module.

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 37-64

Shifting Right
--------------
The other two are *very* similar, so we will only look at the highlights.


Remainder
---------
This looks different from most enum declarations you may have seen:
there are no `=` signs anywhere.
We haven't told Virdant what bit patterns these variants should have --
we've only said there are three of them.

When you leave out the values, Virdant fills them in for you,
using a **one-hot** encoding:
`#ShiftLeftLogical` becomes `0b001`,
`#ShiftRightLogical` becomes `0b010`,
and `#ShiftRightArithmetic` becomes `0b100`.
One bit per variant, exactly one bit set.
The enum type ends up 3 bits wide,
one bit for each variant we declared.

One-hot is a hardware designer's habit.
Decoding `0b010` into "it's the second one" needs no priority logic,
no lookup, no thinking --
just a wire straight off the right bit.
When the actual bit patterns don't matter
(and here they don't: nobody sends a `ShiftDirection` over a wire
to some other RISC-V core),
letting Virdant pick them is free clarity.

(Normally the ``#`` prefix looks odd, but think of it as
"give me the variant, not the variable".)


`word()`: Building Words Out of Pieces
--------------------------------------
Here's a surprise: Virdant has no `<<` or `>>` operators.
Instead, shifting is built out of **slices** and the `word()`
constructor, which glues bit patterns together into a wider word,
most-significant piece first:

.. code-block:: virdant

    word(x, y)    // x on top of y, widths add up

To shift left by 16, drop the top 16 bits and paste 16 zeros
onto the *bottom*.
Here's the first stage of the `ShiftLeft` module,
which owns the whole left-shift network:

.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
    :dedent:
    :lines: 10-20

Read the slice carefully: `value[16..0]` keeps bits 15..0,
the bottom 16 bits. (Slices are **exclusive on top**:
the top index is where you stop, not what you keep.)
Then `word(..., 0w16)` pastes 16 zero bits underneath.
The bits that fell off the top are simply gone --
exactly what a logical shift left should do.


`zext()`: Widening With Zeros
-----------------------------
A right shift is the mirror image:
keep the *top* bits and prepend zeros.

We could prepend with `word()` again --
`word(0w16, value[32..16])` --
but prepending zeros onto a word is so common that
Virdant gives it a name, **zero-extend**:

.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
    :dedent:
    :lines: 43-50

`value[32..16]` keeps bits 31..16, the top half,
and `zext(...)` pads it back out to 32 bits with zeros.
(Why does `value[32..16]` have a `32` in it?
Because the top index is exclusive: a 32-bit word stops at 32.)
The target width of `zext` is inferred from context,
so the same expression would work in a `Word[8]` too.


`mux()`: The Conditional Shift
------------------------------
Of course, we don't want to always shift by 16 --
we want to shift by 16 *when the `amount` asks for it*.
That's a two-way selector, and Virdant's built-in for it is `mux`:

.. code-block:: virdant

    mux(condition, when_true, when_false)

Each stage of our shifter is one conditional shift:

.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
    :dedent:
    :lines: 10-41

Look at the pattern: stage 16 shifts by 16 if `amount[4]` is set,
then stage 8 shifts the *outcome* by 8 if `amount[3]` is set,
then 4, then 2, then 1.
The wire names keep score: `left_4_3` holds the value after
whatever `amount[4]` and `amount[3]` asked for,
all the way down to `left_4_3_2_1_0`, which has heard from all five bits.

Why this works is the whole idea of a **barrel shifter**:
any amount from 0 to 31 is a sum of distinct powers of two --
say, 13 = 8 + 4 + 1 --
so applying a conditional shift for each power of two,
in sequence, composes into any shift amount you like.
Five stages, one per bit of `amount`, and every shift
from 0 to 31 is reachable in a single clock cycle.
The alternatives are worse: shift one bit per cycle (five cycles
at best), or build a 32-way mux over every possible answer.

The right-shift network is the same five stages with the
slices flipped -- keep the high bits, `zext` the zeros in:

.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
    :dedent:
    :lines: 43-71


`sext()`: Widening With the Sign Bit
------------------------------------
Logical right shifts fill with zeros.
But for **signed** numbers, that's wrong:
`-8` shifted right should stay negative,
not turn into a huge positive number.

The fix is **sign-extend**, `sext()`,
which pads with a copy of the value's most-significant bit
instead of zeros:

.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
    :dedent:
    :lines: 74-102

Swap `zext` for `sext` and the right-shift network
becomes the arithmetic right shift.
And it composes correctly, because the sign bit never moves:
at every stage, the bit being replicated into the vacated positions
*is* still the original sign bit.
`-8 >> 1` is `-4`, `>> 2` is `-2`, and so on --
the ones keep streaming in from the top.


One Module Per Direction
------------------------
By now the file has quietly grown three `mod` declarations that
aren't the top module at all: `ShiftLeft`, `ShiftRightLogical`,
and `ShiftRightArithmetic`, each owning exactly one five-stage
network and exposing a single `result`.

Splitting them out keeps each network honest on its own --
`ShiftLeft` is thirty-odd lines that do one thing --
and it leaves the top-level `Shifter` doing what top-level
modules do best: wiring things together and making choices:

.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
    :dedent:
    :lines: 104-137

`mod left of ShiftLeft` creates an **instance** of the
`ShiftLeft` module, and the driver block drives its incoming
ports by name: `it.value`, `it.amount`.
All three networks receive the same operands and work in parallel;
the `match` then picks which submodule's `result` to believe.

This is a **match expression**: inspect `direction`,
pick the arm whose pattern matches,
and the arm after ``=>`` is the value produced.
All arms are examined at once -- this is a mux tree in hardware,
not a chain of tests -- and Virdant insists every variant
of `ShiftDirection` gets an arm.

Notice that only `Shifter` and `ShiftDirection` are marked `pub`.
The three shift modules are private helpers of this package:
nobody outside needs them, so nobody outside sees them.

We'll see much more of `match` -- and of instantiating modules --
in the next chapter, where the ALU wires this very shifter in.


Trying It Out
-------------
Let's shift some bits with ``vir poke``.
Enum variants poke in directly as ``#Name``:

.. code-block:: text

    $ vir poke
    vir poke: top shifter::Shifter (type `exit` to leave, `help` for commands)
    top> poke direction #ShiftLeftLogical
    top> poke value 0x0000abcd
    top> poke amount 0x00000008
    top> assert result == 0x00abcd00

Left shift by 8: `0xabcd` slides up, zeros pour in at the bottom.
Now right, first logically, then arithmetically --
with a value whose sign bit is set:

.. code-block:: text

    top> poke direction #ShiftRightLogical
    top> poke value 0x80000000
    top> poke amount 0x0000001f
    top> assert result == 0x00000001

    top> poke direction #ShiftRightArithmetic
    top> poke value 0x80000000
    top> poke amount 0x0000001f
    top> assert result == 0xffffffff

Same input, same amount, same direction of travel --
but the arithmetic shift spent 31 cycles' worth of stages
filling the word with sign bits.
A negative number stayed negative.

(`assert` complains loudly when its condition is false
and says nothing at all when it is true.)


Where To Next
-------------
The shifter is doing honest RISC-V work, but it has no caller yet.
In the :doc:`next chapter <../alu/index>` we'll drop it into an ALU --
where `funct3` picks the operation,
`funct7[5]` quietly turns SRL into SRA,
and the shifter never even notices the difference.


.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
