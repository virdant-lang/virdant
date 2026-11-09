Struct Types
============
In this lesson, we will demonstrate *struct types*.

`ColorMixer` is a circuit that holds onto a single RGB color and, while
`enable` is asserted, fades it slowly to black:
one color channel per clock cycle, in round-robin order.

.. literalinclude:: /guide/structs/src/structs.vir
    :caption: structs.vir
    :language: virdant


Defining a Struct Type
----------------------
A struct type bundles several named fields into a single value:

.. literalinclude:: /guide/structs/src/structs.vir
    :language: virdant
    :lines: 1-6

Here, `Color` is a new type.
A `Color` value is three 8-bit words: a `red` channel, a `green` channel,
and a `blue` channel.
Field names begin with a lowercase letter and are written in snake case.

.. note::

    In Verilog, related signals like these would be three loose wires,
    passed around separately.
    Any grouping between them is only a convention:
    a comment, a naming pattern, or a hand-rolled concatenation
    whose field order you have to remember.
    A Virdant struct type makes the grouping part of the type system,
    so a `Color` value can never be confused with three unrelated bytes.

A struct type is like a register file with a name:
it is one value, but you can always reach its fields individually,
as we will see below.


Struct Literals
---------------
You create a struct value with the ``${}`` syntax,
assigning each field by name:

.. literalinclude:: /guide/structs/src/structs.vir
    :language: virdant
    :dedent:
    :lines: 24-29

The mixer's `current` register starts out holding
a blue-ish color: `red` is 200, `green` is 128, and `blue` is 255.

A struct literal lists every field of the struct, each with its own value.
We put each field on its own line and line up the ``=`` signs,
which makes the three channel values easy to scan and compare.

Field values do not have to be constants.
Anywhere you would write a driver value, you can build a struct
out of computed expressions, which is exactly what the fade does:

.. literalinclude:: /guide/structs/src/structs.vir
    :language: virdant
    :dedent:
    :lines: 31-38

One enabled cycle at a time, the `phase` register picks which channel
to fade: `phase` is 0 for `red`, 1 for `green`, and 2 for `blue`
(with 3 as a rest cycle, so each channel gets faded once every four clocks).

The inner ``mux(it->red == 0, 0, it->red - 1)`` is a *saturating* decrement:
if the channel is already at zero it stays at zero,
otherwise it drops by one.
Without that check, a fading channel would wrap around from 0 to 255.

Notice that inside the `current` register's driver block,
the struct's own fields are written as `it->red` and so on,
just as `it` refers to the register elsewhere.


Field Access
------------
The ``->`` operator extracts a single named field from a struct value.
For example:

.. literalinclude:: /guide/structs/src/structs.vir
    :language: virdant
    :dedent:
    :lines: 55-58

`red_level` is just the red channel of `current`,
pulled out as an ordinary `Word[8]`.

``->`` also chains through intermediate structs.
The `dimmed` output (below) is itself a `Color`,
and reading its red channel looks like this:

.. literalinclude:: /guide/structs/src/structs.vir
    :language: virdant
    :dedent:
    :lines: 69-72


Struct Equality
---------------
Two struct values of the same type can be compared for equality with ``==``
(and inequality with ``!=``).
Two colors are equal when *all* of their fields are equal.

The mixer uses this to know when the fade is finished:

.. literalinclude:: /guide/structs/src/structs.vir
    :language: virdant
    :dedent:
    :lines: 41-53

`black` is a `Color` whose every channel is zero,
built with a struct literal just like any other value.
Then `at_black` is true exactly when every channel of `current` is zero
-- one comparison instead of three.


A Struct-Producing Output
-------------------------
Combinational logic can produce struct values too.
The `dimmed` output is the current color, halfway to black,
computed channel by channel:

.. literalinclude:: /guide/structs/src/structs.vir
    :language: virdant
    :dedent:
    :lines: 60-67

Each channel is zero-extended to `Word[9]` with `word(false, ...)`,
then sliced with ``[9..1]`` to keep the upper eight bits.
Taking bits 8 down to 1 of a 9-bit value divides it by two,
which is exactly "halfway to black".

.. note::

    Virdant has no ``<<`` or ``>>`` shift operators.
    To shift, slice: ``x[9..1]`` is `x` shifted right by one
    (dropping the top bit), and `zext` `trunc` `word()` handle the
    width changes around it.


Trying It Out
-------------
Here is a poke script that watches the fade happen:

.. literalinclude:: /guide/structs/structs.poke
    :caption: structs.poke
    :language: virdant

The first ``print`` shows the starting color
and its dimmed counterpart, side by side::

    ${red = 0b11001000, green = 0b10000000, blue = 0b11111111}
    ${red = 0b01100100, green = 0b01000000, blue = 0b01111111}

After one enabled tick, `red` has faded from 200 to 199
while `green` and `blue` hold still
-- the assertions on `red_level` and `dimmed_red` check
both the faded channel *and* the value computed from it.
Then we fast-forward through the rest of the fade,
and `at_black` confirms every channel has reached zero.
