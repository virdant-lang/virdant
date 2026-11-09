Platforms
=========

Every FPGA board is different.
It has its own clock oscillator, its own pins, its own LEDs and connectors.
In Virdant, a board is described by a *platform*: a `platform` item which
declares the board's ports, each annotated with the pin it lives on.

A platform is *data*, not code.
Platform items live in library packages (like `ice40` or `virt`), and the
annotations on them are consumed by the build flow, not by `vir check`.
The annotations you will meet are:

- `@fpga("...")` — the FPGA family, which selects the toolchain
  (`ice40`, `xilinx`, ...).
- `@part("...")` — the exact part/package string handed to the
  place-and-route tool.
- `@pin(35)` or `@pin("A1")` — the physical pin of a port.
- `@period_ns("83.33")` — the clock period, on a `Clock`-typed incoming
  port.
- `@noinitial` — marks boards which forbid `initial` statements on
  registers (some real boards have no reliable power-up state).

One more thing a platform can declare is a `default` value for an
`outgoing` port.
We will see what that buys you shortly.

The Virtual Board
-----------------

The easiest platform to try is not a board at all.
`virt::Virt` is a *virtual* FPGA: a design written `for virt::Virt` is
simulated with Verilator through the `virt` tool.
The virtual board emulates a 1 MHz clock, a UART connected to your
terminal's stdin/stdout, and a 16 MiB SPI flash device.

This is the platform's definition:

.. code-block:: virdant

    @fpga("virt")
    @part("virt")
    pub platform Virt {
        //> 1 MHz primary clock.
        @period_ns("1000.000")
        incoming clock : Clock

        //> UART transmit data to the host (stdout).
        outgoing tx : Bit default true
        //> UART receive data from the host (stdin).
        incoming rx : Bit

        //> Program exit request.
        outgoing exit : Valid[Word[8]] default @Invalid()

        //> SPI flash clock.
        outgoing spi_ck : Bit default true
        // ... and the rest of the SPI flash pins ...
    }

A `Hello` for the Virtual Board
-------------------------------

To bind a module to a platform, add a `for` clause to the module
declaration.
Our top module declares `for virt::Virt`, which requires `import virt`
at the top of the file:

.. literalinclude:: /guide/platform/src/hello.vir
    :language: virdant
    :lines: 1-6
    :dedent:

The `for` clause is not documentation: it is *enforced*.
`vir check` checks that the module declares exactly the platform's port
set — same names, same directions, same types.
An extra port is an error, and so is a missing one.

Well — almost.
Each port with a `default` clause may be omitted: the platform then
synthesizes the port for you and drives it with the default expression.
On `Virt`, the `tx`, `exit`, and all four SPI ports have defaults, so a
design which does not use them may simply not mention them.
`rx`, however, has no default.
A module bound to `Virt` must declare it, even if the design never reads
it — and a declared-but-unread port needs an `unused` statement:

.. literalinclude:: /guide/platform/src/hello.vir
    :language: virdant
    :lines: 27
    :dedent:

Now for the design itself.
The virtual board has no LEDs, so our "blink" is the UART line `tx`
itself: a free-running counter whose top bit drives the line, toggling
it roughly once a second:

.. literalinclude:: /guide/platform/src/hello.vir
    :language: virdant
    :lines: 8-17
    :dedent:

Finally, `exit`.
While `exit` reads `@Invalid()`, the simulator keeps running; when it
reads `@Valid(n)` on a clock tick, the simulator exits with code `n`.
Our counter raises `exit` with code `0` the moment it wraps back to
zero, so `vir run` terminates on its own:

.. literalinclude:: /guide/platform/src/hello.vir
    :language: virdant
    :lines: 19-25
    :dedent:

.. note::

    Compared to Verilog, there is no separate constraints file to keep in
    sync with your design.
    The pin assignments *are* the platform definition, and the port match
    against it is checked every time you run `vir check`.

Physical Boards
---------------

`virt::Virt` is one platform among several.
The libraries also ship real boards: `ice40::IceSugar`,
`ice40::IceStick`, `ice40::Fomu`, and the Xilinx boards
`xilinx::ArtyA7` and `xilinx::ArtyA7_100T`.
Here is `ice40::IceSugar`, an iCE40 board with an RGB LED and a user
switch:

.. code-block:: virdant

    @fpga("ice40")
    @part("up5k-sg48")
    pub platform IceSugar {
        //> Primary 12 MHz clock on pin 35.
        @period_ns("83.33")
        @pin(35)
        incoming clock : Clock

        //> RGB LEDs (active low).
        @pin(39) outgoing led_red : Bit default true
        @pin(40) outgoing led_blue : Bit default true
        @pin(41) outgoing led_green : Bit default true

        //> User switch.
        @pin(18) incoming switch0 : Bit
    }

Binding a design to a physical board works exactly like `virt::Virt`:
`import ice40`, write `mod Top for ice40::IceSugar`, and declare the
ports your design needs.
From there, the flow chain `vir build` → `vir synth` → `vir pnr` →
`vir bitstream` → `vir upload` carries the design all the way to the
board, using the `@pin` and `@period_ns` annotations to generate the
constraints.

.. note::

    If your module omits a platform's clock port — say, a board clock
    your design does not use — the synthesized ports do not count as a
    clock for clock inference.
    If the module uses any `reg`, it still needs its own
    `incoming clock : Clock` declaration to drive them.
