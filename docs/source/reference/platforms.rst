Platforms
=========
A platform describes an FPGA board: which FPGA family and part it uses,
and which physical pins its ports are wired to.

A platform is a `platform` item inside a shipped library package, just
like the builtin types ship in the `builtin` package. Platforms are
*data*, not code: the compiler does not validate their contents. The
build flow (`vir bitstream`) reads them when it generates constraints
and invokes the FPGA toolchain, and that is where mistakes in platform
data surface.


The `platform` Item
-------------------
A platform definition declares a named board. Like any item, it may be
preceded by a documentation comment (`//>`) and annotations.

.. code-block:: grammar

    PlatformDef :=
        DocString Annotations "platform" Ident "{" PlatformStmt* "}"

    PlatformStmt :=
        ModDefStmtComponent

The body of a platform contains only port declarations: `incoming` and
`outgoing` components, using the same syntax as module ports. A
platform never declares `wire` or `reg` ports, and its body contains no
drivers, instances, or statements --- a platform is a description of the
board, not of a design.

An `outgoing` port may carry a `default <EXPR>` clause, where `<EXPR>`
is a constant expression (a literal, an enum variant, `@Invalid()`,
`@Valid(n)`, a struct literal, or any expression composed of those).
The compiler does not evaluate the default; it is lowered to Verilog
as-is. See `Platform Defaults`_ below.

.. code-block:: virdant

    @fpga("ice40")
    @part("up5k-sg48")
    platform IceSugar {
        @pin(35) incoming clock : Clock
        @pin(39) outgoing led   : Bit
    }

Item-level annotations describe the board:

* :vir:`@fpga("...")` --- the FPGA family (e.g. :vir:`@fpga("ice40")`).
  This determines which toolchain the build flow dispatches to.
* :vir:`@part("...")` --- the part/package string passed to the place
  and route tool (e.g. :vir:`@part("up5k-sg48")`).

Each port carries annotations describing its physical wiring:

* :vir:`@pin(35)` --- the pin number, or :vir:`@pin("A1")` for boards
  with named pins.
* :vir:`@period_ns(83)` or :vir:`@period_ns("83.33")` --- on a clock
  port, the clock period in nanoseconds, used by the place and route
  tool.

Annotations come *before* the declaration they annotate --- an
annotation wraps the statement that follows it, so stacked annotations
all apply to the same declaration. See :doc:`annotations` for the
general annotation syntax.

Because platforms are data, the compiler performs no validation of
these values: a platform may declare any number of clock ports (or
none), a port may omit its `@pin`, and an `@fpga` family may name a
family no toolchain supports. Nothing is checked at `vir check` time;
invalid platform data produces an error only when the build flow reads
it.


The `for` Clause
----------------
A module declares which platform it implements with an optional `for`
clause, written after the module name and before the body:

.. code-block:: grammar

    ModDef :=
        DocString Annotations "pub"? "ext"? "mod" Ident ForClause? "{" ModDefStmt* "}"

    ForClause :=
        "for" Ofness

    Ofness :=
        Ident | Ident "::" Ident

.. code-block:: virdant

    import ice40

    mod Top for ice40::IceSugar {
        incoming clock : Clock
        outgoing led_red : Bit
        // ...
    }

The clause is optional. A module without a `for` clause is
unconstrained: it may declare any ports (or none), and the platform
checks do not apply to it.

The name after `for` resolves like any other item reference: either a
bare name (:vir:`for IceSugar`) or a `package::`-qualified name
(:vir:`for ice40::IceSugar`), through the module's imports. Writing
:vir:`for ice40::IceSugar` requires :vir:`import ice40` at the top of
the file. See :doc:`packages` for how imports and qualified names work.

`ext` mods cannot meaningfully bind to a platform: they declare ports
but no design, so the port-match check simply ignores them.


Port Matching
-------------
A module with a `for` clause must have *exactly* the platform's port
set: the same names verbatim, the same directions
(`incoming` must match `incoming`, `outgoing` must match `outgoing`),
and the same types.

.. code-block:: virdant

    platform TwoPins {
        incoming clock : Clock
        outgoing led : Bit
    }

    mod Top for TwoPins {
        incoming clock : Clock
        outgoing led : Bit
    }

The match is enforced by `vir check`. It reports an error when:

* a port of the module has no matching platform port (an extra port),
* a platform port has no matching module port (a missing port) ---
  unless the platform port carries a `default` clause, see
  `Platform Defaults`_ below,
* a same-named pair has mismatched directions,
* a same-named pair has mismatched types,
* the name in the `for` clause does not resolve to any item, or
* the name in the `for` clause resolves to an item that is not a
  platform.

Usage of the ports inside the module is enforced exactly as for any
other module: incoming ports must be read or marked :vir:`unused`, and
outgoing ports must be driven.

`vir bitstream` re-verifies the port match before invoking any
toolchain, as a defensive check.


Platform Defaults
-----------------
Asking every module bound to a platform to declare and drive all of its
ports is often boilerplate. An `outgoing` platform port may therefore
declare a `default <EXPR>` clause. When a module with a `for` clause
does not declare that port, the compiler behaves exactly as if the
module had declared it and driven it with the default expression:

.. code-block:: virdant

    platform SpiFlash {
        incoming clock     : Clock
        incoming read_addr : Valid[Word[24]]
        outgoing read_data : Valid[Word[32]]

        outgoing device_ready : Bit        default true
        outgoing spi_ck       : Bit        default false
        outgoing spi_cs       : Bit        default true
        outgoing spi_mosi     : Bit        default false

        incoming spi_miso : Bit
    }

    mod MySpiFlash for SpiFlash {
        incoming clock     : Clock
        incoming read_addr : Valid[Word[24]]
        outgoing read_data : Valid[Word[32]]

        incoming spi_miso : Bit
    }

For every defaulted `outgoing` port the module omits, a synthetic port
is created and driven by the default expression. The synthesized port
is indistinguishable from a hand-written port: every analysis sees it,
it is type-checked against the platform's port type (a mismatching
default expression is an error, reported at the platform's
expression), and it appears in documentation just like any other port.

An `incoming` platform port the module omits is synthesized as a
never-read input, and does not trigger the unused-signal warning.
There is no `default` on `incoming` ports: an incoming port cannot be
driven from the module's side.

A platform `outgoing` port *without* a `default` keeps the old
behavior: a module that omits it gets a missing-port error.

A `default` clause is not allowed on `outgoing` ports of a module
itself --- only in platform definitions. The compiler reports
``DefaultOutsidePlatformPort`` otherwise.


The Shipped ice40 Platform
--------------------------
The `ice40` package ships at `lib/ice40.vir`, next to `lib/builtin.vir`.
Every project loads all `lib/*.vir` files automatically; as with any
package, :vir:`import ice40` puts its items in scope.

It currently defines two boards, the iCESugar and the iCEstick:

.. code-block:: virdant

    //! Platform definitions for the Lattice iCE40 family.

    //> iCESugar board.
    //> Lattice iCE40-UP5K, SG48 package, programmed over USB.
    @fpga("ice40")
    @part("up5k-sg48")
    platform IceSugar {
        //> Primary 12 MHz clock on pin 35.
        @period_ns("83.33")
        @pin(35)
        incoming clock : Clock

        //> RGB LEDs (active low).
        @pin(39) outgoing led_red   : Bit
        @pin(40) outgoing led_blue  : Bit
        @pin(41) outgoing led_green : Bit

        //> User switch.
        @pin(18) incoming switch0 : Bit

        //> UART (USB CDC).
        @pin(4) incoming  uart_rx : Bit
        @pin(6) outgoing  uart_tx : Bit

        //> SPI flash.
        @pin(46) outgoing flash_cs   : Bit
        @pin(48) outgoing flash_clk  : Bit
        @pin(45) outgoing flash_mosi : Bit
        @pin(47) incoming  flash_miso : Bit
    }

    //> iCEstick board.
    //> Lattice iCE40HX-1K, TQ144 package, programmed over USB (FTDI).
    @fpga("ice40")
    @part("hx1k-tq144")
    platform IceStick {
        //> Primary 12 MHz clock on pin 21.
        @period_ns("83.33")
        @pin(21)
        incoming clock : Clock

        //> User LEDs (D1-D5).
        @pin(99) outgoing led1 : Bit
        @pin(98) outgoing led2 : Bit
        @pin(97) outgoing led3 : Bit
        @pin(96) outgoing led4 : Bit
        @pin(95) outgoing led5 : Bit

        //> UART (FTDI USB).
        @pin(9) incoming  uart_rx : Bit
        @pin(8) outgoing  uart_tx : Bit
    }

.. note::

   The nextpnr-ice40 chip flag and package are derived from `@part`
   (see `virdant/src/build/toolchain.rs`): `"hx1k-tq144"` becomes
   `--hx1k --package tq144`.  `vir check` and PCF generation work for
   any ice40 board.

The same package declares external modules for the FPGA's hardware
primitives, annotated with the cell name yosys expects in the generated
Verilog:

.. code-block:: virdant

    //> The iCE40 4 Kbit block RAM.
    //> True dual-port: the read and write ports have separate clocks.
    //> Yosys infers `SB_RAM40_4K` from ordinary memories; instantiate this only
    //> when you need a block RAM you can name directly.
    @cell("SB_RAM40_4K")
    ext mod SbRam40_4k {
        outgoing RDATA : Word[16]
        incoming RCLK  : Clock
        incoming RCLKE : Bit
        incoming RE    : Bit
        incoming RADDR : Word[11]
        incoming WCLK  : Clock
        incoming WCLKE : Bit
        incoming WE    : Bit
        incoming WADDR : Word[11]
        incoming MASK  : Word[16]
        incoming WDATA : Word[16]
    }

The :vir:`@cell("...")` annotation names the Verilog primitive emitted
for instantiations of the ext mod.  The ``ice40`` package declares the
full iCE40 cell library this way: the lookup table and carry cells, the
twenty flip-flop cells (positive and negative edge, with clock enable,
and with synchronous or asynchronous set and reset), the I/O and global
buffer cells, the block and spring RAMs, the PLLs, the UltraPlus
oscillators, LED drivers, hardened I2C and SPI blocks, and the
``SB_WARMBOOT`` and ``SB_MAC16`` cells.  Port names must match the
Verilog primitives verbatim (including their case), since the emitted
instantiation is matched against yosys's own cell library.  Virdant
cannot set Verilog cell parameters yet, so instantiations use each
cell's default parameters.  See :doc:`items` for `ext mod`
declarations.


The Shipped xilinx Platform
---------------------------
The `xilinx` package ships at `lib/xilinx.vir`, next to `lib/ice40.vir`.
It defines two Arty A7 boards, which share one ball assignment (only
the die differs):

.. code-block:: virdant

    //! Platform definitions for the Xilinx 7-series family.

    //> Digilent Arty A7-35T (Rev. D/E).
    //> Xilinx Artix-7 xc7a35t, CSG324 package, programmed over FTDI JTAG.
    @fpga("xilinx")
    @part("xc7a35tcsg324-1")
    platform ArtyA7 {
        //> Primary 100 MHz clock on ball E3.
        @period_ns("10")
        @pin("E3")
        incoming clock : Clock

        //> User LEDs LD0-LD3 (light when driven high).
        @pin("H5")  outgoing led0 : Bit
        // ... led1-led3, buttons, switches, UART, SPI flash ...
    }

    //> Digilent Arty A7-100T (Rev. D/E).
    @fpga("xilinx")
    @part("xc7a100tcsg324-1")
    platform ArtyA7_100T {
        // ... the same ports as ArtyA7 ...
    }

Here `@part` is the full Xilinx part string, and it is passed verbatim
to every tool in the flow: as nextpnr-xilinx's `--device`-naming chipdb
(`xc7a35tcsg324.bin`, the part with its speed grade dropped), as
fasm2frames's `--part`, and as xc7frames2bit's `--part_name`.

`vir bitstream` runs the Xilinx 7-series flow:

1. Yosys: ``synth_xilinx -flatten -abc9 -arch xc7``, producing a JSON
   netlist in `build/`.
2. nextpnr-xilinx with an XDC constraints file emitted from the
   platform's `@pin` annotations:
   ``set_property LOC E3 [get_ports clock]`` lines, an
   ``IOSTANDARD LVCMOS33`` per port (the Arty A7's banks are 3.3 V),
   and one ``create_clock -period 10 [get_ports clock]`` per clock
   port.
3. Project X-Ray's frame tools convert nextpnr's FASM output to
   frames and pack the frames into ``build/<project>.bit``.

This flow needs more environment than the ice40 flow: `nextpnr-xilinx`
(the openXC7 toolchain's shim or an equivalent build), the
``fasm2frames`` and ``xc7frames2bit`` programs from Project X-Ray, a
nextpnr-xilinx chipdb for the part, and the Project X-Ray database.
Two environment variables point the build flow at the last two:

* ``NEXTPNR_CHIPDB_DIR`` --- a directory of nextpnr-xilinx chip
  databases; ``xc7a35tcsg324.bin`` (or ``chipdb-xc7a35tcsg324.bin``)
  is expected there for the Arty A7-35T.
* ``PRJXRAY_DB_DIR`` --- the Project X-Ray database root, whose
  ``artix7/xc7a35tcsg324-1/part.yaml`` must exist (this is the same
  variable the openXC7 toolchain's ``export.sh`` sets).

A missing tool or environment variable fails before the affected
external tool runs, naming the variable.

`vir upload` flashes with openFPGALoader
(``openFPGALoader -b arty_a7_35t`` for the Arty A7-35T,
``-b arty_a7_100t`` for the Arty A7-100T).


Building and Flashing
---------------------
The `Virdant.toml` file selects which module `vir bitstream` synthesizes:

.. code-block:: toml

    [project]
    name = "blink"

    [prog]
    top = "Top"

`vir bitstream` then:

1. Resolves the top module named by `[prog] top`. A missing
   `[prog] top` key is an error before any external tool is invoked.
2. Resolves the top module's `for` clause to its platform, and
   re-verifies the port match as a defensive check (normally already
   enforced by `vir check`). A top module with no `for` clause is an
   error here, before any external tool is invoked.
3. Emits Verilog for the design into `build/`.
4. Emits a constraints file in the platform family's format from the
   `@pin` annotations and the clock's `@period_ns`: a PCF for ice40,
   an XDC for xilinx.
5. Runs the toolchain (yosys, nextpnr-ice40, icepack for ice40;
   yosys, nextpnr-xilinx, fasm2frames, xc7frames2bit for xilinx) and
   writes `build/<project>.bin` or `build/<project>.bit`.

`vir upload` builds the bitstream and flashes it with the board's
programmer, inferred from the top module's platform (e.g. `icesprog`
for IceSugar, `iceprog` for IceStick, `openFPGALoader` for Arty A7).

`vir pnr` re-runs only place-and-route and packing over a design that
`vir bitstream` has already synthesized, reusing the netlist at
``build/<project>.json`` instead of paying for yosys again. Because
nextpnr's simulated-annealing placer is stochastic --- the same netlist
can close timing on one seed and miss on another --- this is the tool
for fighting a marginal timing closure:

.. code-block:: text

    vir pnr --tries 8 --seed 1

Each attempt runs nextpnr with a different ``--seed`` (the first seed
is ``--seed``, later attempts increment it), and the first attempt that
closes wins. Every attempt's output is kept in ``build/nextpnr/`` as
``<project>.<seed>.asc`` (ice40) or ``<project>.<seed>.fasm`` (xilinx),
failed attempts included, for inspection; the winning attempt is
copied to ``build/<project>.asc``/``<project>.fasm`` and packed as
usual. ``--allow-timing-failure`` passes nextpnr's
``--timing-allow-fail``, so a timing miss no longer fails the run and
the first attempt wins even if it misses timing. Like `vir bitstream`,
`vir pnr` still resolves the platform and re-emits the constraints, so
source edits are reflected in the constraints --- but the netlist is
not resynthesized, so run `vir bitstream` first whenever the design
itself changed.

Only the ice40 and xilinx families are supported. The `@fpga` value
selects the toolchain at build time, so a platform naming an
unsupported family passes `vir check` and fails in `vir bitstream`
with a clear error.

A blink project bound to the IceSugar board ties off every unused
output:

.. code-block:: virdant

    import ice40

    mod Top for ice40::IceSugar {
        incoming clock : Clock
        outgoing led_red   : Bit
        outgoing led_blue  : Bit
        outgoing led_green : Bit
        incoming switch0 : Bit
        // ... the remaining platform ports ...

        reg counter : Word[24] on clock {
            it <= it + 1
        }

        led_red := ~counter[23]
        led_blue := 1
        led_green := 1

        unused switch0
    }

.. note::

   `vir new <project>` scaffolds a `Virdant.toml` and a `src/top.vir`.
   The generated `Virdant.toml` sets `[prog] top`, but the generated
   `Top` module has a single `led : Bit` port and no `for` clause, so
   it is not yet bound to a board and will not build with `vir bitstream`
   as-is. Add a `for` clause (and matching platform ports) as shown
   above before building.


Adding a Platform
-----------------
Because a platform is a `.vir` file rather than code, the work depends
on whether the FPGA family is already supported.

A new board in a supported family (today: ice40 or xilinx) is pure
data: add one
`platform` item to `lib/ice40.vir` or `lib/xilinx.vir` with the
item-level `@fpga` and `@part` annotations, one `@pin(...)` per port,
and a `Clock`-typed
incoming port with `@period_ns(...)`. Nothing else is needed ---
constraint generation and toolchain dispatch are keyed off the `@fpga`
family. A single lib file may declare several `platform` items.

A new FPGA family needs, in addition, code in the build flow keyed by
the `@fpga` family string: a toolchain runner and flasher, and a
constraint emitter for the family's constraint format.
