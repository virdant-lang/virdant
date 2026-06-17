SPI Controller
==============
In this next example, we will demonstrate enum types.

`SpiController` is a circuit which sends data to a device
using the :tip:`SPI (Serial Peripheral Interface)` protocol.

.. literalinclude:: /examples/spi.vir
    :caption: spi.vir
    :language: virdant


Sending Data
------------
To send data using `SpiController`, we use two ports:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :dedent:
    :lines: 11-12

You place the byte you want to send into `data`,
and then you set `valid` to `true`.

The SPI protocol allows us to send one bit at a time.
This means that `SpiController` takes several cycles to send the data.
During this time, it is not ready to receive another byte to send,
and it will ignore its inputs until it's ready again.

You can tell whether the `SpiController` is ready by looking at the value of the `ready` port:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :dedent:
    :lines: 14-16

We'll look more into this syntax shortly.

The `SpiController` will begin a transaction as soon as *both* `ready` and `valid` are asserted.

Serial Protocol Interface
-------------------------
Serial Protocol Interface is a 4-wire protocol for communicating data to a device:

.. list-table::
    :header-rows: 1

    * - Port
      - Name
      - Description
    * - `sck`
      - serial clock
      - the clock used to synchronize the controller and device
    * - `cs`
      - chip select
      - whether a transaction is in progress (active-low)

        aka `ss` (slave select)
    * - `do`
      - data out
      - the bit of data being sent by the controller

        aka `mosi` (master out, slave in)
    * - `di`
      - data in
      - the bit of data being received by the controller

        aka `miso` (master in, slave out)


.. note::

    For the sake of simplicity, our `SpiController` example is designed to only send data, not to receive it.
    And so, it includes `sck` `cs` and `do` but omits `di`.

When a transaction begins, `SpiController` pulls `cs` low (sets it to `false`).
This is how you "wake up" a device using SPI.

The `SpiController` then alternates between setting `do` (the data out) to the next bit of data
and ticking `sck` (the serial clock).


Once the transaction is complete, `SpiController` brings `cs` back high,
signaling the end of the transaction.


Enum Types
----------
The `SpiController` is a state machine.
When it is idle, it waits for you to initiate the next transaction.
And once a transaction is in progress,
it alternates between setting up the next bit to send and actually sending it with a clock tick.

To represent these state values, we use a feature of Virdant called enum types:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 1-6

We use `enum type` to define a new enum type.
Inside the definition, we list all of the values in the type,
together with their numeric representations.

The first entry for an `enum type` must also specify the width.
Here, this is done with the syntax `0b001w3`.
The `0b` means the number is specified in binary,
the `001` gives the value,
and the `w3` at the end means that this is a 3-bit value.
(The leading `0`\s in `001` are just for visual effect and don't affect the bit width).

The *values* of our new type are prefixed with a ``#``.
They are: `#Idle` `#Set` and `#Tick`.

Enum types are very useful for when we want to list all of something in one place.
Here are just a few examples where we might use them:

- states of a state machine (as we are doing here)
- opcodes for a protocol (eg, `Get` `Ack` `PutFullData` etc of TileLink)
- opcodes for an instruction set (eg, `LOAD` `STORE` `OP` `BRANCH`, etc in RISC-V)
- CPU privilege modes (eg, `Machine` `Supervisor` `User`)
- Exception reasons (eg, `ILLEGAL_INSTRUCTION` `DIV_BY_ZERO` `PAGE_FAULT`, etc)

In `SpiController`, we track the current state in the `state` register,
and we give it the type `SpiState`.


Equality Testing
----------------
In Virdant, we can compare values with `==`.
This makes it easy when we want to ask if we're in a particular state.

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 14-16
    :dedent:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 22-24
    :dedent:

And as you might expect, we use `!=` to test that two values are *not* equal:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :lines: 18-20
    :dedent:


State Machines
--------------
The `SpiController` also defines how `state` changes over time.
This is done with a driver that uses `when` and `match` expressions:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :dedent:
    :lines: 32-39

We see a `when` expression at the top which handles the reset logic.
Resetting the controller puts it into `#Idle` mode.

Next, inside the `else` branch, we see a `match` expression which looks at `state`.
Inside, we see several `case`\s, some with `mux` expressions.

So in total, this code says:

  "Look at the current `state` and then, on the next cycle, transition to the appropriate next state."


Notice how each case uses `mux` expressions to decide the next state.
For example, in the `#Idle` case, we transition to `#Set` if `valid` is true, otherwise stay in `#Idle`.

This pattern of using expressions to describe state transitions is cleaner than
using imperative statement blocks.

Note that this behavior *only* works for `<=` (latched drivers).
When using `:=` (continuous drivers), we must *always* have full case coverage.
Virdant will flag an error if you forget.

.. note::

    In guides on Verilog, you will often read of strict prescriptions on how to write state machines.
    This has to do with the subtleties of Verilog's non-blocking assignments
    and latch inference behaviors.

    Luckily, we do not have to worry about these things in Virdant.


Shifting out Data
-----------------
The `buffer` register is used to keep track of what data is left to be sent.
The `do` port is simply an alias for the top bit of `buffer`:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :dedent:
    :lines: 26-28


The definition of `buffer` itself comes a bit later:

.. literalinclude:: /examples/spi.vir
    :language: virdant
    :dedent:
    :lines: 44-48


We see that it latches the value of `data` when a transaction begins.
If we're not in `#Idle` state or `valid` is false, the buffer retains its current value.

On each `#Tick`, we shift it over 1 bit.
We do this using bit slicing: `buffer[7..0]`.
This returns the lower 7-bits of `buffer`.

The first index (`7`) is exclusive, while the second index (`0`) is inclusive.
This makes it easy to calculate at a glance the result is `7` - `0` = 7 bits, or `Word[7]`.

.. note::

    This convention is closer to the convention found in programming languages.

    For experienced Verilog developers, this will take some getting used to.
    Luckily, Virdant's type system will quickly alert you to where you have made a mistake.


Clocks
------
We end by looking at a subtle point about clocks.

In `SpiController`, we see `clock` has type `Clock`, but `sck` has type `Bit`.
What's the difference?

The `Clock` type is special in Virdant because they affect the timing of our design.
The only things we can do with `Clock`\s are:

- pass them around, and
- feed them to the `on` clause of a `reg`

However, we *cannot* use operators like `&&` `||` `!` or apply any other logic to them.
`Clock`\s must be carefully regulated so we don't end up with bad things like
clock skew or timing violations.

SPI is a protocol for talking to off-chip peripherals.
If we synthesize our design and put it onto an FPGA,
we fully expect `sck` to end up going out to an IO port, not to any registers.
While timing concerns are still important, as far as Virdant is concerned,
we can treat it like a standard value.
And so we use `Bit` instead.
