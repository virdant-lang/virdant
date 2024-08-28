Type Schemes
============
It is possible to define new types in Virdant.

Each user-defined type belongs to one of the following type schemes:


Struct Types
============
A `struct type` is a type which breaks down into several fields.

For example, suppose you were writing hardware which made heavy use of 24-bit RGB color values.
You could encode a color with `Word[24]`.
However, this has the disadvantage that the designers have to keep track of the color encoding in their head.

Instead, Virdant allows you to define a new struct type `Color` as:

.. literalinclude:: /examples/rgb.vir
    :language: virdant
    :dedent:
    :lines: 1-5

You can construct values of a struct type with the syntax `$Color { red = 0, green = 0, blue = 0 }`.

If you are given a value of type `Color`, you can project out each field using the syntax `color->red`, etc.


Union Types
===========
A `union type` is what's sometimes known as an algebraic data type.
Some programming languages also call them `enum` types.

Union types are useful when defining state machines.

Union types are defined with a set of constructors.
Each constructor takes zero or more arguments.
The type of each argument is given when defining the type.

Here is an example of a definition for an union type:

.. literalinclude:: /examples/gcd.vir
    :language: virdant
    :lines: 1-5

We define `State` to have three constructors: `Idle`, `Running`, and `Done`.

You construct values of a union type by calling the constructors, prepended with an `@` sign.
For example, `@Idle()` is a `State`, as is `@Done(15)` or `@Running(7, 8)`.

To make use of a value with a union type, you can match on it with a `match` statement:

.. literalinclude:: /examples/gcd.vir
    :language: virdant
    :dedent:
    :lines: 16-29

Here, the subject of the match, `state`, has type `State`.
The body of the match statement consists of three match arms.
Each one starts with a pattern:

* `@Idle() => ...`
* `@Running(x, y) => ...`
* `@Done(result) => ...`

And each ends with an expression.

The patterns may introduce new variable bindings.

* `@Running(x, y)` introduces `x` and `y`, which both have type `Word[8]`.
* `@Result(result)` introduces `result` which has also type `Word[8]`.

Unions help communicate intent that certain data is only "valid" in certain states.
In this example, the `x` and `y` variables are only valid when the machine is running.
And so Virdant prevents you from even accessing these values otherwise.

So, in English, the whole match statement above says:

* If the state is idle, start it running with values `a` and `b`. (These are ports of the module).
* If the state is running, subtract the larger or `x` and `y` from the other, or halt if `y` is zero.
* If the state is done, move it on to idle after one cycle.


Enum Types
==========
When we want to capture a number of values symbolically,
such as a set of opcodes, we use an `enum type`.

.. literalinclude:: /examples/opcodes.vir
    :language: virdant
    :dedent:
    :lines: 1-7

Each enum types are declared with an explicit `width`, indicating the number of bits needed to represent each value.
Then, each enumerant is given an explicit value.

To create a value of an enum type, you can use the syntax `#AND`.
You can also match against an enum type:

Just like with union types, you can pattern match on enumerants.

.. literalinclude:: /examples/opcodes.vir
    :language: virdant
    :dedent:
    :lines: 15-21


To get the underlying value, you can use the `word()` construct.
For example, `word(#XOR)` evaluates to the value `3`.
