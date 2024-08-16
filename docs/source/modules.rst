Modules
=======
Modules are the basic unit of hardware.

Modules consist of a list of statements.
These do things like:

* declare ports to allow the module to communicate with the outside world
* declare registers for tracking the state of the module
* instantiate other modules as submodules
* declare logic to perform on the registers and ports

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

Let's look at the `Buffer` module definition.

This module has two ports, an incoming port `inp` and and outgoing port `out`.
Both carry a value of type `Word[4]`.
This is how we say "a 4-bit value" in Virdant.

This module contains one subcomponent: a `reg` named `queue`, also with type `Word[4]`.

The then have two driver statements.
The first driver, `queue <= inp`, will latch the value of `inp` into the register `queue` on every clock cycle.
The second wire, `out := queue`, will connect the register `queue` to the outgoing port `out`.

The second module is `Top`.

We see it declares two ports, `clock` and `out`, and a register `counter`.
The driver statement for `counter` says that every cycle, `counter` is incremented.


Submodule Instances
-------------------
The next statements `mod buffer of Buffer` declares a new submodule instance.
This acts as if we have a copy of `Buffer` inside of `Top`.
We give this submodule the name `buffer`.

A module can only access a submodule through its ports.
Moreover, a module is obligated to drive all `incoming` ports of its submodules.

After declaring `buffer`, we must drive its `clock` and `inp` ports.
We do this by using driver statements: `buffer.clock := clock` and `buffer.inp := counter`.

A module may use the `outgoing` ports of its submodules in expressions.
Thus, our last statement drives the outgoing port `out` with `buffer.out`.
