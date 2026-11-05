Full Adder
==========
Our `HalfAdder` is good enough to add any pair of 1-bit numbers together.
However, if we want to sum up numbers with more than 1 bit, we need to handle carries.

`FullAdder` is a standard circuit which is a variation of `HalfAdder` that does this.

.. literalinclude:: /examples/adder/src/adder.vir
    :language: virdant
    :lines: 15-37

In addition to `a` and `b`, `FullAdder` takes an additional 1-bit input called `carry_in`.
The circuit then adds `a`, `b`, and `carry_in` together and returns their sum as `carry_out` and `sum`.

The way `FullAdder` works is by making use of two `HalfAdder`\s!

First Half Adder Submodule
~~~~~~~~~~~~~~~~~~~~~~~~~~
The first `HalfAdder` is this:

.. literalinclude:: /examples/adder/src/adder.vir
    :language: virdant
    :dedent:
    :lines: 22-27

The `mod` keyword, when used inside a module definition, creates an *instance* of another module.
Here, we give the submodule the name `ha0`.

.. note::

    As you may have guessed, `ha` stands for "half adder".
    While heavy abbreviation is very common in digital design,
    in Virdant, you are encouraged to use long, descriptive names.
    Perhaps `half_adder0` would have been an even better choice!

    Also, it is conventional to start numbering starting at `0`.
    So our "first" `HalfAdder` is not `ha1` but rather `ha0`.

We create a driver block to connect up the ports of `ha0`.
The variables `a` and `b`, of course, refer to the `incoming` ports of our `FullAdder` module.
The `it.a` and `it.b` (or equivalently, `ha0.a` and `ha0.b`) are the `incoming` ports of our `HalfAdder` submodule.
By connecting these as shown, we are passing the inputs `a` and `b` down to the `HalfAdder`.

Once we do this, `ha0` will begin summing `a` and `b` for us.
The sum of `a` and `b` are made available as `ha0.carry_out` and `ha0.sum`.

Second Half Adder Submodule
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our second `HalfAdder` submodule is this:

.. literalinclude:: /examples/adder/src/adder.vir
    :language: virdant
    :dedent:
    :lines: 28-33

The difference here is that we are not summing `a` and `b`, but rather `ha0.sum` and `carry_in`.
Since `ha0.sum` equals `a` + `b`, this will have the effect of summing `a` + `b` + `carry_in`, just as we wanted!

Wrapping up the Full Adder
~~~~~~~~~~~~~~~~~~~~~~~~~~
The two remaining lines of `FullAdder` drive the outputs.

.. literalinclude:: /examples/adder/src/adder.vir
    :language: virdant
    :dedent:
    :lines: 35-36

We see the use of a logical OR to compute the `carry_out` in the expression `ha0.carry_out || ha1.carry_out`.
