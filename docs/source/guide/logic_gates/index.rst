Logic Gates
===========
The heard of all digital hardware are logic gates: AND OR NOT and XOR.

`LogicGates` is a simple circuit which shows how you write each of these in Virdant:

.. literalinclude:: src/logicgates.vir
    :caption: logicgates.vir
    :language: virdant
    :lines: 1-20

The module takes two 1-bit operands, `a` and `b`.
We use the `Bit` type to indicate they are 1-bit values.

Driver Blocks
-------------
The `outgoing` ports of `LogicGates` are written a bit differently than before.
For example:

.. literalinclude:: src/logicgates.vir
    :caption: logicgates.vir
    :language: virdant
    :lines: 5-7
    :dedent:

This is called a driver block.
Inside a driver block, we can write driver statements.
When we do, the special `it` keyword refers to the component being declared.
In this example, `it` becomes shorthand for `a_and_b`.

Driver blocks let you type less, which is always nice.
You can just write `it` instead of the full name of the component.
We'll see later on how they end up saving a *lot* of typing!

But perhaps more importantly, they keep the margin of the module definition tidy.
As you scroll through the code,
your eyes are drawn towards the component declarations: `incoming a` `incoming b`
`outgoing a_and_b` `outgoing a_or_b` etc.
You don't have to worry about getting stuck on the details of how a component is driven.
Only once you find the thing you're actually interested,
then you can look in the block to see how it's defined.


Driver blocks are optional, and a matter of style but highly encouraged.


Next
----
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

.. literalinclude:: src/logicgates.vir
    :caption: logicgates.vir
    :language: virdant
    :lines: 22-49


Word-Wise Bitwise Operators
---------------------------
The first family works on whole words, bit by bit:
`&` is bitwise AND, `|` is bitwise OR, and `^` is bitwise XOR.

.. literalinclude:: /guide/operators/src/operators.vir
    :language: virdant
    :lines: 17-20
    :dedent:

With `a = 0b1010` and `b = 0b0110`,
`a & b` is `0b0010`, `a | b` is `0b1110`, and `a ^ b` is `0b1100`.

.. note::

    These are the same symbols Verilog uses for bitwise operations,
    and `~x` inverts every bit of a word, just like Verilog's `~`.
    One difference to watch: in Virdant, `&` `|` and `^` bind at the
    same level as `+` and `-`, so `a & b == 0` parses as
    `(a & b) == 0`.
    In C and Verilog, `==` binds tighter than `&`,
    giving the surprising `a & (b == 0)`.
    When in doubt, use parentheses.


Bit-Wise Logical Operators
--------------------------
The second family works on single bits:
`&&` is logical AND, `||` is logical OR, `^^` is logical XOR,
and `!` is logical NOT.

.. literalinclude:: /guide/operators/src/operators.vir
    :language: virdant
    :lines: 22-30
    :dedent:

Here we apply them to the low bits of `a` and `b`, and to `select`.
With `a[0] = 0`, `b[0] = 0`, and `select = true`,
every result is `0` except `!select`.

.. note::

    These four operators require `Bit` operands: writing `a ^^ b`
    on two words is a type error.
    Verilog has no separate logical operators on single bits;
    it reuses `&`, `|`, `^`, and `~` for both purposes,
    which is exactly the ambiguity Virdant avoids by having two
    distinct families.


Comparisons
-----------
The comparison operators all produce a `Bit`:
`==`, `!=`, `<`, `<=`, `>`, and `>=`.

.. literalinclude:: /guide/operators/src/operators.vir
    :language: virdant
    :lines: 32-35
    :dedent:

We pack all six results into one word so that a single value tells the
whole story.
With `a = 0b1010` (10) against `b = 0b0110` (6), only `>`, `>=`,
and `!=` come out `1`, so `comparison` reads `0b110010`.

Verilog uses the same six symbols, but a comparison there is just a
1-bit value you can do arithmetic on.
In Virdant, a comparison is honestly a `Bit`, and the type system
reminds you.


Arithmetic
----------
The arithmetic operators `+` and `-` add and subtract two words of the
same width, and the result wraps to that width.

.. literalinclude:: /guide/operators/src/operators.vir
    :language: virdant
    :lines: 37-40
    :dedent:

Watch what happens with `a = 0b1010` and `b = 0b0110`:

* `a + b` is `16`, which does not fit in 4 bits, so it wraps to `0`.
* `a - b` is `4`.
* `a +. b` is the *widening* add: the operands stay 4 bits wide, but
  the result is 5 bits wide, and the extra top bit carries the
  carry-out: `0b10000`, which is `16` with no information lost.

There is a matching `-.` for subtraction with a borrow-out.
Use the wrapping forms when overflow cannot happen or does not matter,
and the widening forms when you need to know.

.. note::

    Virdant does not (yet!) have a `*` operator for multiplication.
    Multiplication is built out of the operators above, one shift and
    one add at a time; see the :doc:`multiplier <../multiplier/index>`
    and :doc:`multicycle multiplier <../multicycle_multiplier/index>`
    lessons for two ways to do it.


Selection
---------
The `mux` primitive chooses between two values.

.. literalinclude:: /guide/operators/src/operators.vir
    :language: virdant
    :lines: 42-45
    :dedent:

`mux(select, a, b)` is `a` when `select` is `true` and `b` otherwise.
The first argument must be a `Bit`, and the other two must have the
same type.

.. note::

    Verilog writes `select ? a : b`.
    The C-style ternary survives in Verilog; in Virdant,
    selection is a named primitive.
    When the choice grows beyond two arms,
    reach for `if` or `match` blocks instead.


Bit Manipulation
----------------
The last family moves bits around rather than computing with them.
`word(...)` concatenates its arguments into one wider word, listed
from most-significant to least-significant, and `x[hi..lo]` slices a
window out of a word.

.. literalinclude:: /guide/operators/src/operators.vir
    :language: virdant
    :lines: 47-50
    :dedent:

`concatenated` is the top half of `a` stacked over the low half of `b`.
With `a = 0b1010` and `b = 0b0110`,
`a[4..2]` is `0b10` and `b[2..0]` is `0b10`,
so `concatenated` is `0b1010`.

Remember that slices are exclusive on top: `x[hi..lo]` covers bits
`hi - 1` down through `lo`, so `a[4..2]` is two bits wide, not three.
There is no `<<` or `>>` in Virdant; shifting is done by slicing and
re-concatenating, as the :doc:`barrel shifter <../barrelshifter/index>`
lesson shows in depth.

.. note::

    Verilog concatenates with `{a, b}` and slices with `a[3:2]`,
    where *both* endpoints are inclusive.
    Virdant's `word(a, b)` and `a[4..2]` do the same jobs, but the
    slice convention differs, so porting a Verilog slice always
    deserves a second look.


Trying It Out
-------------
Because the design is purely combinational, we can drive it and check
it without ever ticking a clock.
The demo script pokes two operand scenarios and asserts every packed
result:

.. literalinclude:: /guide/operators/operators.poke
    :caption: operators.poke

Run it from this directory with:

.. code-block:: console

    $ vir poke operators.poke --script

In the first scenario, `a = 0b1010`, `b = 0b0110`, and `select = true`:
the word-wise bitwise results pack into `0b0010_1110_1100`,
the six comparisons into `0b110010`,
and `10 + 6` wraps to `0` in the low four bits of `arithmetic`
while the widened `10 +. 6` keeps the full `0b10000`.
In the second scenario, `a = 0b0011`, `b = 0b0101`, and
`select = false`: the roles flip, `3 - 5` wraps to `0b1110`,
and `mux` now selects `b`.

Once you can combine, compare, compute, choose, and concatenate,
you have every tool this guide's arithmetic lessons use.
Next, we put a few of them to work in a real design.
