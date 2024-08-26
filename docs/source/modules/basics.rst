Module Basics
=============
Modules are the basic unit of abstraction in Virdant.
We'll look at modules in detail with the following example:

.. literalinclude:: /examples/buffer.vir
   :caption: buffer.vir
   :language: virdant
   :linenos:


Module Definitions
------------------
Modules are declared using the `mod` keyword.
We can see that in our example, there are two modules defined: `Top` and `Buffer`.
The order of module definitions does not matter.

Let's look at the first module definiton, `Buffer`.


Ports
-----
Ports are signals that allow communication between the module and the outside world.

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 2-4

In Virdant, each port has a directionality, either `incoming` or `outgoing`, a name, and a type.

.. note::

   Verilog uses the keywords `input` and `output` rather than `incoming` and `outgoing`.
   Virdant chooses the keywords it does because it makes the ports line up nicely.
   This makes the code much easier to read, without asking the designer to add extra spaces or tabs into their code.

Our example has three ports: `clock` `inp` and `out`.

The first port is an incoming port named `clock`.
It has type `Clock`.
Virtually every Virdant module will include a clock.
We named it `clock` by convention and place it at the very top of the module definition.
We will use this clock with our register below.

The second port is an incoming port named `inp`.
It has type `Word[4]`.
This is how we say "a 4-bit value" in Virdant.
The value of `inp` is provided by the outside world.
We may use `inp` in any expressions in the module definition.

The third port is an outgoing port named `out`.
It also has type `Word[4]`.
It is a value we are sending out to the outside world.
We are obligated to drive a value to it inside of the module.


Registers
---------
Registers are the stateful elements of Virdant.
They are declared with the keyword `reg`.

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 16

Our `Buffer` module contains one register named `queue`.
It has with type `Word[4]`.

Each register is assocaited with a clock.
We create this association with the `on` clause.
The expression must be a signal of type `Clock`.

The value contained in a registers is updated rising edge of its clock.

Like `incoming` ports, we can use the name of the register in any expression to read its current value.


Drivers
-------
The next two statements are called driver statements.
Driver statements supply the value to a component.

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 7

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 9

The first driver, `queue <= inp`, will latch the value of `inp` into the register `queue` on every clock cycle.
The second wire, `out := queue`, will connect the register `queue` to the outgoing port `out`.
You will notice these use different operators.
The `<=` is used with registers, and it reminds us that we are assigning to the new value of the register.
On the other hand, `:=` is a continuous driver, and it tells us that the right hand side is equal to the left hand side at any moment in time.


The Top Module
--------------
Each design will name a module definition as its "top" module.
By convention, we name this module `Top`.

.. tip::

   If you come from a programming background,
   you can think of `Top` being analogous to `main`.

Sure enough, we see that the second module definition in our example is named `Top`,
so we can presume this is meant to be the "entrypoint" of our design.


Counters
--------
While not a feature of modules per se, counters are a common occurrence in hardware.
Our `Top` module declares a register named `counter` like this:

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 16-17

The driver sets `counter` to the value of `counter->inc()` on every cycle.
The syntax `counter->inc()` is a method call.
It's short for "increment", and it gives the next value up,
wrapping around to `0` if the value is the maximum value.

.. note::

   Note that methods and other expressions in Virdant cannot change the value of any register on their own.
   Only driver statements can assign values to hardware.
   If you come from a programming background, this is similar to the idea of a "pure function".


Submodule Instances
-------------------
The power of modules is that they allow us to design our hardware heirarchically.
This means that a module may instantiate a another module as a submodule.
When we do so, we set up a parent-child relationship between the two.

In Virdant, this is done with the syntax:

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 19

This declares a new submodule named `buffer` inside of the `Top` module.
The submodule is created from the `Buffer` module definition,
and the submodule acts as if we have a copy of `Buffer` inside of `Top`.

A parent module is only allowed to communicate with its child through the child's ports.
That means that a parent cannot see the insides of its submodules:
the registers or any sub-subinstances are off limits.
This provides encapsulation in your hardware design and promotes modular design.

The parent module is obligated to drive all of the `incoming` ports of its submodules.
This is what we see on the next few lines:

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 20-21

As you can see, we refer to the ports of a submodule instance by a dotted path
indicating the name of the submodule and the name of the port.

In return for driving the `incoming` ports, the parent may make use of the values of the `outgoing` ports.
We see an example of this in the expression a few lines later, where we use `buffer.out`:

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 26-30

After declaring `buffer`, we must drive its `clock` and `inp` ports.
We do this by using driver statements: `buffer.clock := clock` and `buffer.inp := counter`.

A module may use the `outgoing` ports of its submodules in expressions.
Thus, our last statement drives the outgoing port `out` with `buffer.out`.


Wires
-----
It can sometimes be handy to give a name to an expression in a module definition.
This is especially useful for when a value is used repeatedly inside of a module definition.
It is also good because it communicates your intent as a designer and makes your code more readable.

Virdant supports the `wire` keyword for this:

.. literalinclude:: /examples/buffer.vir
   :language: virdant
   :dedent:
   :lines: 23-24

In this example, we declare a wire named `counter_is_zero`.
It has type `Bit`, which is a 1-bit value.

.. tip:

   If you come from programming, this is similar to the `bool` type found in programming languages.

The value is given by the driver statement.
The `eq` method asks if `counter` is equal to the value `0` (and returns `true` if it is and `false` if it isn't).


And so concludes our example.
