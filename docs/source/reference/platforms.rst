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
        DocString Annotations "ext"? "export"? "mod" Ident ForClause? "{" ModDefStmt* "}"

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
* a platform port has no matching module port (a missing port),
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

   The build flow's nextpnr invocation currently hardcodes the
   `--up5k` chip flag (see `virdant/src/build/toolchain.rs`), so
   `vir bitstream` only place-and-routes correctly for the iCESugar
   today. `vir check` and PCF generation work for any board, including
   the iCEstick, but driving nextpnr for a non-up5k chip (like the
   iCEstick's hx1k) needs that chip flag derived from `@part` instead
   of hardcoded.

The same package declares external modules for the FPGA's hardware
primitives, annotated with the cell name yosys expects in the generated
Verilog:

.. code-block:: virdant

    //> iCE40 Block RAM primitive.
    @cell("SB_RAM40_4K")
    ext mod SbRam40_4k {
        incoming clock : Clock
        incoming addr  : Word[14]
        incoming wdata : Word[8]
        incoming wmask : Bit
        outgoing rdata : Word[8]
    }

The :vir:`@cell("...")` annotation names the Verilog primitive emitted
for instantiations of the ext mod. See :doc:`items` for `ext mod`
declarations.


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
4. Emits a PCF constraints file from the platform's `@pin` annotations
   and the clock's `@period_ns`.
5. Runs the toolchain (yosys, nextpnr-ice40, icepack) and writes
   `build/<project>.bin`.

`vir upload` builds the bitstream and flashes it with icesprog.

Only the ice40 family is supported. The `@fpga` value selects the
toolchain at build time, so a platform naming an unsupported family
passes `vir check` and fails in `vir bitstream` with a clear error.

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

   `vir new <project>` scaffolds a `Virdant.toml` and a `src/top.vir`,
   but its current output predates the `for`-clause binding scheme
   described above: the generated `Virdant.toml` sets `[prog] platform`
   (not `[prog] top`), and the generated `Top` module has a single
   `led : Bit` port and no `for` clause. Such a project is not
   bound to a board and will not build with `vir bitstream` as-is;
   edit both files by hand into the `[prog] top` / `for` form shown
   above before building.


Adding a Platform
-----------------
Because a platform is a `.vir` file rather than code, the work depends
on whether the FPGA family is already supported.

A new board in a supported family (today: ice40) is pure data: add one
`platform` item to `lib/ice40.vir` with the item-level `@fpga` and
`@part` annotations, one `@pin(...)` per port, and a `Clock`-typed
incoming port with `@period_ns(...)`. Nothing else is needed ---
constraint generation and toolchain dispatch are keyed off the `@fpga`
family. A single lib file may declare several `platform` items.

A new FPGA family needs, in addition, code in the build flow keyed by
the `@fpga` family string: a toolchain runner and flasher, and a
constraint emitter for the family's constraint format.
