Hello World
===========
This is the design you get when you create a new Virdant project.
After running ``vir new hello``, the freshly-created ``hello/src/top.vir`` looks
exactly like this:

.. literalinclude:: src/top.vir
    :caption: top.vir
    :language: virdant


Platforms
---------
Working with hardware is tricky because every setup is totally different.
Even when using the same FPGA, what development board you're using matters.
It affects how the synthesis, place and route, and programmer need to be configured.

It is beautiful that everyone has their own setup.
But for learners, it's good to have something that just works so they can get started.

A platform in Virdant represents a piece of hardware (usually a particular FPGA)
embedded in a particular environment (usually a development board).

We declare that a module targets a certain platform like this:

.. code-block:: virdant

    pub mod Top for virt::Virt

The `for` keyword says that this is a top module *for* our platform.
And in this case, the platform we choose is `virt::Virt`, the Virdant Virtual platform.

It's called the Virtual platform because it doesn't require any hardware.
Instead, `Virt` allows you to simulate your design.

If you have Virdant installed, try it:

.. code-block:: console

    $ vir new hello
    Created project 'hello'
    $ cd hello/
    $ vir run
    Hello, world!

Hello, indeed!

Working with `Virt`
-------------------
Every platform defines a set of ports.
Marking your module with `for` tells Virdant to check
that you don't make up, misspell, or forget any ports.
It also wires them up properly when you simulate or flash your device.

.. list-table::
    :header-rows: 1
    :widths: 12 12 22 20

    * - Direction
      - Port
      - Type
      - Default
    * - `incoming`
      - `clock`
      - `Clock`
      -
    * - `outgoing`
      - `exit`
      - `Valid[Word[8]]`
      - `@Invalid()`
    * - `outgoing`
      - `debug`
      - `Valid[Word[8]]`
      - `@Invalid()`
    * - `outgoing`
      - `tx`
      - `Bit`
      - `true`
    * - `incoming`
      - `rx`
      - `Bit`
      -
    * - `outgoing`
      - `spi_ck`
      - `Bit`
      - `true`
    * - `outgoing`
      - `spi_cs`
      - `Bit`
      - `true`
    * - `outgoing`
      - `spi_mosi`
      - `Bit`
      - `true`
    * - `incoming`
      - `spi_miso`
      - `Bit`
      -

In our example, we make use of the `clock` `debug` and `exit` ports on `Virt`.
We'll explain each of those.

We will see the remaining ports in later examples.
`tx` and `rx` let us send and receive data over UART.
`spi_ck` `spi_cs` `spi_mosi` and `spi_miso` are for connecting to the SPI flash

We must declare the platform ports we plan to use, just like any other module.
We are not required to declare a port if we don't plan to use it.
For `outgoing` ports, the platform will usually have a default value.

The Clock
---------
`clock` is our clock.
Pretty much every platform has one.
What matters most is the *speed* of the clock.
`Virt` runs nominally at 1 MHz.

.. note::

    If your design is too big,
    `Virt` will run much slower than 1 MHz.
    However, the simulator's logic still assumes that everything is running at 1 MHz.
    But your design will feel like it is in slow motion.

Printing with `debug`
---------------------
Programmers are spoiled with ``print`` statements.
Hardware has none of that.
But sometimes you really just want to print *something* to the screen.
For this, we have the `debug` port.

.. literalinclude:: src/top.vir
    :caption: top.vir
    :language: virdant
    :lines: 18-20
    :dedent:

The `debug` port is also a `Valid[Word[8]]`.
When it is driven with `@Valid(char)`,
the simulator will print the character with ASCII code `char` to standard error.


Exiting the Simulation
----------------------
Normally a design on an FPGA runs indefinitely until the power is turned off.
For our simulator, though, it's very convenient to be able to signal from inside the design
to signal that we're ready to end.

For this, we use `exit`:

.. literalinclude:: src/top.vir
    :caption: top.vir
    :language: virdant
    :lines: 22-27
    :dedent:

`exit` is a `Valid[Word[8]]`.
The simulation continues to run as long as you drive it with `@Invalid()`.
But as soon as you drive it with some value `@Valid(n)`,
the simulation terminates and returns the exit code `n` to the operating system.

By convention, a nonzero exit code means "something went wrong!"
And so using `exit`, we can write simple tests.

In our Hello World example, we will exit
(with exit code `0`, meaning "success")
when `char` is equal to zero.

.. note::

    In the C programming language, strings are represented by an array of bytes.
    But the very last byte must be a zero byte, written ``'\\0'``.
    That's how C knows when to stop printing the string.

    We're doing something similar here, as we'll see next.


Memories
--------
In addition to registers, Virdant has a second stateful primitive: memories.
Memories are for when you need to store larger amounts of data.
They are declared with the `memory` keyword:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 6-7
    :dedent:

The name of our memory is `mem`.
The `64` in `mem[64]` means that this memory holds 64 values.
The type, `Word[8]`, means that each value is a `Word[8]`.

Registers and memories serve slightly different purposes in hardware.
A register is typically implemented as a flip-flop
(or "flop" if you're being slick).
On an FPGA, each logic tile has one.
The FPGA on my desk has a bit over 5000 flip-flops.

But in total, that amounts to less than 1 KiB of state.
For most applications, you need at least a few kilobytes,
if not a handful of megabytes.
And so FPGAs will also provide dedicated memory.
This is often block :tooltip:`RAM (Random-Access Memory)`.

`memory` components in Virdant are platform-specific.
But Virdant will use them efficiently for your platform.

Initializing Memories
---------------------
The line `@image_inline("Hello, world!\\n\\r\\0")` is an annotation.
Annotations allow you to supply metadata to a Virdant program.
In this case, the `@image_inline` tells Virdant
"when you simulate the design, initialize the memory with this data".

.. note::

    On some platforms, `@image_inline` can also be used to initialize the memory.
    You can also use `@image_file` to initialize the data with a file.

Reading from Memories
---------------------
Here is how we read from a memory:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 14-16
    :dedent:

The `dyn` in `mem[dyn index]` reminds us that this is a dynamic index.
(This is more hardware than a static index, so we always have to say `dyn`).

If it helps to think of it this way,
`index` is playing the role of a memory address.


Waveform
--------
Up to this point, waveforms have been the primary way we interact with our designs.
Now that we have platforms, though, we can actually start running our designs for real.

Of course, when things go wrong on hardware, nothing is better than a waveform.
For small designs, you can produce a :tooltip:`VCD (Value change dump



Let's land this example by watching the whole thing run in the waveform viewer.
This design runs itself, so there is nothing to poke:
we simply tick the clock once per character with `run.poke`:

.. code-block:: console

    $ vir poke run.poke

And here is the run from time zero to the very end.
Follow `index` from left to right:
it starts at `0` and counts up by one every cycle.
Each cycle, `debug` carries the next character as its ASCII code:
`@Valid(72)` is `H`, `@Valid(101)` is `e`, and so on.
And at `index` `15`, we reach the zero byte,
and `exit` switches to `@Valid(0)`.
That is the exact moment the simulation ends, with exit code `0`: success.

.. vcd:: trace.vcd

    label Clock
    signal top.clock:clk

    label Memory access
    signal top.index:dec

    label Platform ports
    signal top.debug:Valid[Word[8]]
    signal top.exit:Valid[Word[8]]

