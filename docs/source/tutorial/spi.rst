SPI Controller
==============
In this next example, we will demonstrate enum types.

`SpiController` is a circuit which sends data to a device
using the Serial Peripheral Interface (SPI) protocol.

.. literalinclude:: /examples/spi.vir
    :caption: spi.vir
    :language: virdant
    :linenos:


How it Works
------------
The `SpiController` is a state machine with three states: `#Idle` `#Send` and `#Clock`.
It tracks the current state in the `state` register.

The `SpiController` is connected to a SPI device using three pins:

- `clk` (clock)
- `cs` (chip select), and
- `do` (data out)

This example assumes the device only receives data and does not send it,
so we do not include the `di` (data in) pin you normally find on SPI.

When you want to send data to the connected SPI device,
you put the data on the `data` port and set `valid` to signal the start of a request.

On the next clock cycle after initiatng a request,
the `SpiController` will go from `#Idle` to `#Send` and latch `data` into `buffer`.

Over the next several clock cycles, the `SpiController` will shift it out the data over the `do` pin.
It does so one at a rate of one bit every other cycle,
as it oscillates between `#Send` and `#Clock`.
During this time, it will pull `ready` low so you know it's busy.

Once it has shifted all the bits of `buffer` out (as tracked by `bits`),
it returns to the `#Idle` state.


Enum Types
----------
In Virdant, you can define new types.
To create a new type which defines a number of named constants,
we use `enum type`:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 1-6


The SPI controller is a state machine.
We define a new type called `SpiState` which represents the four states it can be in.

We declare the type to have `width 4`, so we know it is represented with 4 bits.
And we give each enumerant a numeric value.

As we will see, we use `#Idle` `#Send` `#Clock` and `#Done` to refer to the values of this type.


Clocks
------
By now, we've seen a number of module definitions.
But let's point out a few things about `SpiController`.

First, we seem to have two clock: `clock` vs `clk`.
What is the difference?

If we look closely, `clock` has type `Clock`,
so it is the clock for the module.
The other signal, `clk` has type `Bit`.
It is the SPI clock signal.

The `Clock` type is special in Virdant because they affect the timing of our design.
The only things we can do with `Clock`\s are:

- pass them around, and
- feed them to the `on` clause of a `reg`

However, we *cannot* use operators like `&&` `||` `!` or apply any other logic to them.
`Clock`\s must be carefully regulated so we don't end up with bad things like
clock skew or timing violations.

SPI is a protocol for talking to off-chip peripherals.
If we synthesize our design and put it onto an FPGA,
we fully expect `sck` to end up going out to an IO port, and not to any registers.
And so we use `Bit` instead.


Equality Testing
----------------
In Virdant, we can compare values with `==`.
This makes it easy when we want to ask if we're in a particular state.

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 12-14
    :dedent:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 24-26
    :dedent:


And as you might expect, we use `!=` to test that two values are *not* equal:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 20-22
    :dedent:

By the way, `cs` stands for "chip select".
It is pulled low (set to `false`) to wake up the peripheral.

Is this the most natural way to write this?


State Machines
--------------
The reason we defined `SpiState` above was so we could use this powerful pattern:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 30-52
    :dedent:

The `state` register keeps track of the current state.

After checking `reset`, we see a large `match` block.
This takes the current value of `state` (referred to as `it`, since we're in a driver block),
and decides on the *next* value of `state` based on it.

In the language of finite state machines,
this gives the *transition table* for the `SpiController`.
Virdant's syntax makes it easy to work out what the next state will be.

The first `case`, for instance, says that the way to transition from `#Idle` to `#Send` is
when `valid` is true.
