Counter
=======
In this next example, we will demonstrate how to use registers, the stateful components in Virdant.

.. literalinclude:: /examples/counter.vir
    :caption: counter.vir
    :language: virdant
    :linenos:

In this example, we declare, four components: `clock` `reset` `out` grouped together, and also `counter` a bit below.


Types
-----
Virdant has a rich type system.
This example introduces a few new types.

`Clock` is used for clock signals in Virdant.
All digital circuits make use of a clock.
In a typical FPGA, clocks can run at anywhere from 10MHz to 500MHz.
Since `Clock` signals are what synchronize the circuit, they get their own special type.

`Bit` is used for 1-bit values.
The two possible values are `true` and `false`.

`Word[4]` is a 4-bit data value.
This allows our counter to count from 0 up to 15, since 15 is the largest 4-bit number.


Registers
---------
The heart of our `Counter` module is a register:

.. code-block:: virdant

    reg counter : Word[4] on clock

Every `reg`\s requires an `on` clause which tells you which clock it runs on.
Here, `on clock` tells us that `counter` will update whenever `clock` is pulsed.

If Statements
-------------


.. literalinclude:: /examples/counter.vir
    :language: virdant
    :dedent:
    :lines: 7-11


Extra
-----
Note that we group the first three together and leave an empty line before declaring `counter`.
Beauty is one of the :doc:`Principles of Virdant </principles>`.
By visually splitting these up, it helps focus our attention to `counter` as the central theme of the module.
We also have a degree of freedom in how we organize our blocks.
We make use of this to express not only the *logic* of the design, but also how we want people to *feel* about it.
