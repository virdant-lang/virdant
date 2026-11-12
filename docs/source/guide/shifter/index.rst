Shifter
=======
In this next example, we'll define our own type,
and learn about a handful of smaller features that are good to know about.

`Shifter` is a barrel shifter, a circuit that performs bit shifting:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 7-35

Here we have our shifter module.
It has three `incoming` ports: `direction` `value` and `amount`,
and one `outgoing` port: `result`.


Operation of `Shifter`
----------------------
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
`0b1000_0000 >> 3 = 0b0001_0000`.

But there's also an equally good argument for filling it with...
whatever the top bit is:
`0b1000_0000 >> 3 = 0b1111_0000`.

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
Let's go over the `ShiftLeft` module:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 37-56

Wires
-----
Once we understand one of these drivers, we can understand the whole thing:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 41-43
    :dedent:

This component is one we haven't seen before.
It's a `wire`.
We use `wire` when we want to give a name to a value.
We drive it it using `:=`.

Words with Widths
-----------------
If we squint, we'll see nestled in this driver a value written `0w4`.
This is how we say `0` but with 4-bits.
The `w` stands for "of width".

.. note::

    In Verilog, you would write this :verilog:`4'b0`.

Concatenation
-------------
If we zoom out, we see the expression `word(value[4..0], 0w4)`.
The `word` function will convert each of the arguments to a `Word` type,
and then, it will concanenate them all together into one long `Word` type.
In this case, we take the first 4 bits of `value` and add on 4 `0` bits.
But another way, this expression gives us the result of shifting `value` 4 bits to the left.

Mux
---
Finally, we see an the full expression:
`mux(amount[2], word(value[4..0], 0w4), value)`.
The `mux` function (short for multiplexer) is a compact way of writing simple `if` statement.

Putting this all together,
`left_2` is a value which is either:

- `value` 4 bits to the left (if bit 2 of `amount` is set)
- otherwise it is `value` with no shifting at all

Operation of `ShiftLeft`
------------------------
Now that we have seen all of the pieces,
let's zoom out and look at the structure of the module.

We think of the 3-bit input `amount` as 3 requests:

- if bit 2 is set, please shift left by 4
- if bit 1 is set, please shift left by 2 more
- if bit 0 is set, please shift left by 1 more

Each of the three wires represents the result after the given requests:

.. list-table::
    :header-rows: 1

    * - Wire
      - Value
    * - `left_2`
      - `value` shifted (or not) by 4 (based on bit 2)
    * - `left_2_1`
      - `value` shifted (or not) by 2 more (based on bit 1)
    * - `left_2_1_0`
      - `value` shifted (or not) by 1 more if needed (etc)

The final `result` is just an alias for `left_2_1_0`,
which is when we have taken each of the three bits of `amount` into account.

`ShiftLeft` Waveform
--------------------
Normally, we would save the waveforms for the end,
but let's look at this one so we understand it.
We'll just shift `1` by various amounts
(but we'll write the amounts in binary to make it easier to see).
Each bit of `amount` is one stage's request:
`0b111` is all three requests firing at once.

.. vcd:: trace.vcd

    signal top.value:bin
    signal top.amount:bin
    space
    signal top.left_2:bin
    signal top.left_2_1:bin
    signal top.left_2_1_0:bin
    signal top.result:bin

Watch the cascade do its work:
whenever `amount` gains a set bit,
the corresponding wire slides `value` further up the row,
pasting zeros in underneath,
and the wires *below* it in the list inherit the shift.
By the bottom of the figure, `result` is just
`left_2_1_0` doing its final pass.


Shifting Right
--------------
The other two are *very* similar, so we will only look at the highlights.

From `ShiftRightLogical`, our drivers look like this:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 62-64
    :dedent:

`zext` is short for zero extend.
It takes a value and pads it on the left with `0`'s.

From `ShiftRightArithmetic`, our drivers look like this:

.. literalinclude:: src/shifter.vir
    :language: virdant
    :lines: 83-85
    :dedent:

The only difference is we use `sext` this time,
which is short for sign extend.
It takes a value and pads it on the left with whatever the most significant bit is.

Full Code Example
-----------------
Without further commentary, here is the full code example:

.. literalinclude:: src/shifter.vir
    :caption: shifter.vir
    :language: virdant
