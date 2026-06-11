Muxes and Decoders
==================
Now, we will demonstrate how to use conditional expressions through a series of small, related examples.

Multiplexer
-----------
`Mux2` is a module which takes two inputs and selects between them.


.. literalinclude:: /examples/mux2.vir
    :caption: mux2.vir
    :language: virdant

`Mux2` is a simple module which takes in two signals `a` and `b` and a control signal `select`.
It outputs `a` if `select` is asserted and `b` otherwise.


When Expressions
----------------
We use `when` to select between two different values based on conditions.
The syntax uses `case` for each condition and `else` for the default.


It Blocks
---------
You can also use `it` blocks to assign to the implicit `it` variable
within an outgoing port declaration.

Here is an alternative version:

.. literalinclude:: /examples/mux2_alt.vir
    :caption: mux2_alt.vir
    :language: virdant

Which do you find more natural?

.. note::

    Verilog uses the syntax `select ? a : b` borrowed from C.

    Virdant uses `when` expressions which can handle multiple conditions cleanly.


Decoder
-------
`Decoder4` takes a number as input and outputs the "one-hot encoding" of that number.

.. literalinclude:: /examples/decoder4.vir
    :caption: decoder4.vir
    :language: virdant

`Decoder4` takes a 2-bit number `inp` as input.
It's output, `out`, is a 4-bit number whose bits are all zero
*except* for the position given by the `inp`.


Match Expressions
-----------------
Because we are selecting between multiple alternatives based on a value, we use `match`.

A `match` expression looks at the value you give
and tries to match it against all of the different `case`\s.
When it finds one that matches, the result becomes the value given on the right hand side of the `=>`.

One really nice thing about match expressions is that they prevent you from "forgetting" to cover every case.


Literals
--------
Since we are outputting a binary value,
it felt like it would make a lot of sense to use Virdant's syntax for binary numbers: `0b0001`.


Match with It Blocks
--------------------
Just as you can use `when` inside an `it` block,
you can also use `match` expressions inside `it` blocks.

.. literalinclude:: /examples/decoder4_alt.vir
    :caption: decoder4_alt.vir
    :language: virdant

I think in this case, I like the `match` expression version!
However, it's very common to use `match` statements in more complicated examples.
