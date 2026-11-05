4-bit Adder
===========
The joy of the `FullAdder` is that they can be chained together to add numbers of any size.

We define a module `Adder4`, which can add together any two `Word[4]`\s:

.. literalinclude:: /examples/adder/src/adder.vir
    :language: virdant
    :lines: 39-73

We will leave some of the details of this module for you to explore,
but here are some features to look out for.

Indexing
~~~~~~~~
Both `a` and `b` have type `Word[4]`.
But the inputs to our `FullAdder`\s is `Bit`.
In order to add corresponding pairs of bits, we need to get access to them.

This syntax `a[0]` means "from the word `a`, extract bit number `0`".

When adding numbers by hand, we line up the digits of the two numbers vertically.
In the same way, in `Adder4`, we line up corresponding pairs of bits:

- `a[0]` and `b[0]`
- `a[1]` and `b[1]`
- `a[2]` and `b[2]`
- `a[3]` and `b[3]`


Concatenation
~~~~~~~~~~~~~
The opposite of indexing is concatenation.
This is used on the last line of `Adder4`.

Virdant uses the syntax `word(a, ..., z)` to concatenate the values `a` `b` `c` ... `z` together.
You can concatenate as many values as you like,
and the values you concatenate can be `Bit`\s or `Word`\s.

The result of `cat(a, ..., z)` will be a `Word` type.
And its size will be computed based on the sum of the sizes of each of the arguments.

For example, because each of the arguments is of type `Bit`,
the expression `word(fa3.sum, fa2.sum, fa1.sum, fa0.sum)` is `Word[4]`.
This is good, since it's being assigned to `out`, which is also `Word[4]`.


Chaining Full Adders
~~~~~~~~~~~~~~~~~~~~
The key of the `Adder4` design is the four `FullAdder`\s which are chained together:

.. literalinclude:: /examples/adder/src/adder.vir
    :language: virdant
    :lines: 59-63
    :dedent:

To power each `FullAdder`, we must drive its inputs,
sending the corresponding pair of bits to `it.a` and `it.b`.
We also mix in the `carry_out` of the previous `FullAdder` to the `carry_in` of the next.

I wonder what the `carry_in` of the first `FullAdder` should be?


The Complete Example
--------------------
.. literalinclude:: /examples/adder/src/adder.vir
    :caption: adder.vir
    :language: virdant
