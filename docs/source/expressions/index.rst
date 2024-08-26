Expressions
===========
Expressions represent combinational logic.


Literals
--------
Literals are hard-coded constants.

To represent `Bit`\s, we use `true` and `false`.

To represent values of type `Word[n]`, we annotate constants with their bitwidth.
The literal `42w16` is a 16-bit integer with the value 42.

We may also elide the width in any context where the type can be inferred.
For example, if we define a register:

.. code-block:: virdant

    reg counter : Word[4] on clock;
    counter <= 0;

The value of `counter` is written `0`, but is inferred as if we had written `0w4`.


References
----------
A reference is a name that points to something that carries a value.

Most references point to components.
Components include ports, registers, the ports of submodules, and the channels of a socket.

References of local ports and registers are just simple identifiers, like `clock` or `out` or `counter`.
References to ports of submodules and channels of a socket will be dotted expressions: `buffer.clock`, `buffer.inp`, etc.

Since using a reference implies reading it, when a reference points to a component,
it must correspond to a component which sources values.
You can reference a `reg` or an `incoming` port, but you cannot reference an `outgoing` port.

For clarity, when referencing a `reg`, it results to the *previous* value of the register, as opposed to the *next* value.
This means a statement such as `counter <= counter->inc()` is taking the previous value of `counter`, incrementing it,
and then that value gets latched to supply `counter` with its next value.

In certain expressions, references can also point to locally-bound variables.
For example, in the match expression:

.. code-block:: virdant

    match maybe_data {
        @Invalid => 0;
        @Valid(payload) => payload;
    }

The reference `payload` on the right hand side of the `=>` references the locally-bound variable (of the same name)
declared inside the pattern on the left hand side: `@Valid(payload)`.


Methods
-------
A type may define methods, similar to many programming languages.

The syntax is `subject->method(arg1, ..., argn)`.

The type of the subject must be inferrable.

The method supplies a type signature which gives each argument an expected type, as well as the return type.

Some important methods include:

* `a->inc()` increment `a`
* `a->dec()` decrement `a`
* `a->add(b)` add `a` and `b`
* `a->sub(b)` subtract `b` from `a`
* `a->not()` logical NOT `a`
* `a->and(b)` logical AND `a` with `b`
* `a->or(b)` logical OR `a` with `b`
* `a->xor(b)` logical XOR `a` with `b`
* `a->eq(b)` test if `a` equals `b`
* `a->neq(b)` test if `a` does not equal `b`
* `a->gt(b)` test if `a` is greater than `b`
* `a->lt(b)` test if `a` is less than `b`
* `a->get(i)` dynamically index into `a` to get bit `i`


Concatenation
-------------
You can concatenate words with the syntax `word(a, b)`.

The syntax is variadic, and so you can write things like `word(a, b, c, d)`.
Each argument of `word` must have an inferrable type.
The type of each one must be one of `Word`, `Bit`, or of an enum type.
The result will have a `Word` type with its width equal to the sum of all the widths of all the arguments.

The bits of the result are ordered such that early arguments are placed in the higher-order bits.
So `word(1w1, 0w3)` will result in the value `0b1000`.

In addition to concatenation, you can use `word` to cast a value to a `Word` type.
For example, `word(false)` will result in the value `0` and has type `Word[1]`.
You can also use it on values of enum types.

As a piece of trivia, you can also write `word()` to result in `0` with type `Word[0]`.


Indexing
--------
You can statically index into a `Word[n]` with the syntax `w[0]`.

The index must be a constant value.
Note that we use a plain literal and not `0w8` here.
Moreover, it must be in the range of `0` to `n - 1`.

The result has type `Bit`.


Slice Indexing
--------------
You can also slice a word with the syntax `w[8..6]`.

The two indexes must be literal integers.
Both must be in the range of `0` to `n - 1`.
Moreover, the high index comes first and must be greater than or equal to the lower index.

.. warning::

    Note that this is totally different from how Verilog indexes.
    It was chosen so that we preserve the ordering of the indexes (high bit first),
    but otherwise mirrors what is conventional in most programming languages.

The result of a slice index is `Word[k]` where `k` is the difference between the high and low index.
In the example `w[8..6]`, the result is a `Word` type with width 8 - 6.
Thus, the result is `Word[2]`.


If Expressions
--------------
`if` expressions be used to create mux trees with one or more conditions.
All `if` expressions must have an `else` branch.

.. literalinclude:: /examples/conditions.vir
    :language: virdant
    :dedent:
    :lines: 6-10


Note that the syntax does not require parentheses around the condition,
but it *does* require curly braces.

If you want to test multiple conditions, you write `else if`.
Conditions are checked in order, as you would expect.


Match Expressions
-----------------
`match` expressions allow you to select an expression based on a result.

A match statement can be used to break a value apart and then analyze the pieces.
Think of it as a powerful version of the `if` statement.
It works nicely with union types and enum types.

Here is an example:

.. literalinclude:: /examples/matches.vir
    :language: virdant
    :dedent:
    :lines: 6-9

You may optionally place a colon and a type to give a type ascription here:

.. literalinclude:: /examples/matches.vir
    :language: virdant
    :dedent:
    :lines: 13-16


Type Ascription
---------------
In some cases, you may want to supply the type of a value explicitly.

Type asription in Virdant is written `e[Foo]`.
This causes the expression `e` to be checked against type `Foo`.

This is required in some cases where the typechecker needs to be able to infer an expression's type,
such as when using `word` on an enumerant literal.
For example, you cannot simply write `word(#xor)`, since the arguments of `word` must be inferrable,
but there might be multiple enum types with an `#xor`.
So instead, we write `word(#xor[AluOp])`

Type ascription is also very handy when you run into a type error you don't understand,
and you want to figure out where the source of the issue is.
It's also an alternative way to be explicit about the width of a literal.
Eg, `0w8` can also be written `0[Word[8]]`.
