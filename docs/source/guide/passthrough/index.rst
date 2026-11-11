Passthrough
===========
Modules are the building blocks of digital circuits.
Let's start by see how to define a module in Virdant.

`Passthrough` is a module which takes in some data and immediately sends it back out unchanged.

.. literalinclude:: /guide/passthrough/src/passthrough.vir
    :caption: passthrough.vir
    :language: virdant


Modules are declared using the `mod` keyword, followed by the name of the module.

The body of the module consists of a set of declarations about what the module contains.

.. note::

    This module definition is also marked `pub`, which is short for "public".
    We will learn about `pub` later on.


Ports
-----
Ports are signals that allow communication between the module and the outside world.

.. literalinclude:: /guide/passthrough/src/passthrough.vir
    :language: virdant
    :dedent:
    :lines: 2-3

In Virdant, each port has a direction, either `incoming` or `outgoing`, a name, and a type.

.. note::

    Verilog uses the keywords :verilog:`input` and :verilog:`output` rather than `incoming` and `outgoing`.
    Virdant chooses the keywords it does because it makes the port names line up nicely.
    This makes the code much easier to read,
    without asking you to add extra spaces or using tabs into your code.

Our example has two ports: `inp` and `out`.

The first port is an incoming port named `inp`.
It has type `Word[8]`.
This is how we say "an 8-bit value" in Virdant.
The value of `inp` is given to the module from the outside world.

The second port is an outgoing port named `out`.
It also has type `Word[8]`.
It is a value we are sending out to the outside world.
The module is obligated to drive the value of `out` using a driver statement.

.. note::

    The `out` port is marked `dependson inp`,
    which means the value of `out` depends on the value of `inp`.
    Virdant will tell you if you get it wrong,
    so we won't worry about this until later.


Drivers
-------
The last statement in our module is a driver statement:

.. literalinclude:: /guide/passthrough/src/passthrough.vir
    :language: virdant
    :dedent:
    :lines: 5

A driver statement tells Virdant what value a component will have.
The left hand side is the component we are driving.
The right hand side is an expression, which determines the value we drive.

In our case, we are driving `out` using the expression `inp`.
In other words, we are taking the input to our module and simply sending it back out, unchanged.
So while our `Passthrough` module doesn't do very much, it *does* pass data through.

.. note::

   The `:=` driver in Virdant is analogous to the `assign` keyword in Verilog.


Waveform
--------
What does this module actually do though?
Let's take a look at a run through the waveform viewer:

.. vcd:: trace.svg

    label Input
    signal top.inp:dec

    label Output
    signal top.out:dec

When looking at a waveform,
time advances from left to right.

Here, we see our two signals.
We are treating `Passthrough` as our top module,
so their names are prefixed with `top`.

We imagine someone from the outside passing in different values
into our circuit as `top.inp`.
How does `top.out` respond?

.. note::

    You can hover over the waveform diagram with your mouse.
    The yellow line shows you what the value of every signal is
    at that exact moment in time.

So how do you feel about the simplicity of Virdant so far?
It turns out that Simplicity is one of the :doc:`Principles of Virdant </principles>`!
