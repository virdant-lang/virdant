Buffer
======
In this next example, we will demonstrate how to use registers, the stateful components in Virdant.

`Buffer` is a module which takes in some data and then, on the next clock cycle, sends it back out.

.. literalinclude:: /examples/buffer.vir
    :caption: buffer.vir
    :language: virdant

In this example, we declare four components: `clock` `inp` `out` grouped together, and also `buffer` a bit below.


Types
-----
`Clock` is used for clock signals in Virdant.
All digital circuits make use of a clock.
In a typical FPGA, clocks can run at anywhere from 10MHz to 500MHz.
Since `Clock` signals are what synchronize the circuit, they get their own special type.

`Word[32]` is a 32-bit data value.
In general, we can make `Word` types of any fixed size.


Registers
---------
The heart of our `Buffer` module is a register:

.. code-block:: virdant

    reg buffer : Word[32] on clock

Every `reg`\s requires an `on` clause which tells you which clock it runs on.
Here, `on clock` tells us that `buffer` will update whenever `clock` is pulsed.

Driver Blocks
--------------
Just after `buffer` is declared, we see a block containing two drivers:

.. literalinclude:: /examples/buffer.vir
    :language: virdant
    :dedent:
    :lines: 6-9

This is called a driver block.
This is a way to grouping our drivers to make the code easier to read.

Inside of a driver block, we may use the keyword `it` as a shorthand for referencing the component being declared.
So in our example, `it` is an alias for `buffer`.
This reduces the amount of typing we have to do.

Latched vs Continuous Drivers
-----------------------------
We also notice in our driver blocks that there are two different kinds of drivers: `<=` and `:=`.

The `<=` driver is called a **latched driver**.
It is only used when the left hand side is a `reg`.
It indicates that the value will be latched by the register on the next clock cycle.

The `:=` driver is called a **continuous driver**.
It is used for all other components *except* for `reg`.
And since `out` is an `outgoing` (and not a `reg`), we use `:=` instead of `<=`.
This syntax indicates that the value of the lefthand side is always equal to the expression on the righthand side.

.. note::

    The `<=` syntax is *not* Verilog's non-block assignment.
    Virdant has no notion of blocking vs non-blocking assignment.


Using Driver Blocks
-------------------
Driver blocks give you a pleasant ability to express yourself in Virdant code.

The example we gave above emphasizes that `buffer` is the centerpiece of our `Buffer` module.
However, we could have written it a few different ways.

Here is another way that comes to mind:

.. code-block:: virdant
    :caption: buffer.vir

    mod Buffer {
        incoming clock : Clock
        incoming inp : Word[32]

        outgoing out : Word[32] {
            it := buffer
        }

        reg buffer : Word[32] on clock {
            it <= inp
        }
    }

This style keeps each component's driver close to where it's declared.

Or you could do it without driver blocks like we saw with the `Passthrough` example:

.. code-block:: virdant
    :caption: buffer.vir

    mod Buffer {
        incoming clock : Clock
        incoming inp : Word[32]
        outgoing out : Word[32]
        reg buffer : Word[32] on clock

        buffer <= inp
        out := buffer
    }

This style helps emphasizes the components themselves,
treating their definitions as details only for those who are curious.

Can you think of any other ways to group the drivers?
Which do you like best?
