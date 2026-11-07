# Platforms (FPGA boards)

A `platform` item describes a board: FPGA family/part annotations plus the
board's pinned ports. It lives in a library package (`lib/*.vir`), not in
your design source.

- Platforms are DATA, not code: `vir check` does not validate platform
  contents (`@fpga`, `@part`, `@pin` values are unchecked). Bad platform
  data only errors when the build flow reads it (`vir synth`/`pnr`/
  `bitstream`/`upload`; see ./tools.md).
- What `vir check` DOES enforce: the port match between a module and its
  platform (below).

## Binding a module: the `for` clause

- Write `mod Top for ice40::IceSugar`. Requires `import ice40` at the
  top of the file (qualified name goes through imports).
- Bare name (`for IceSugar`) resolves too; `package::Name` avoids
  ambiguity. The platform must be `pub` in its package to be referenced
  from another package.
- No `for` clause = unconstrained module: any ports, no platform checks.
- `ext` mods ignore the port-match entirely (an `ext mod` cannot
  meaningfully bind to a platform).

```virdant
import ice40

mod Top for ice40::IceSugar {
    incoming clock : Clock
    outgoing led_red : Bit

    reg counter : Word[24] {
        it <= it + 1
    }

    led_red := !counter[23]
}
```

## Port matching (enforced by `vir check`)

A module with `for` must have EXACTLY the platform's port set:

- Same names verbatim, same directions (`incoming`↔`incoming`,
  `outgoing`↔`outgoing`), same types.
- Extra module port => error
  (`Module has port extra which platform IceSugar does not declare`).
- Missing module port => error (`Module is missing port no_default
  required by platform Buttons`) — UNLESS the platform port has a
  `default` clause (below).
- Mismatched direction or type on a same-named pair => error.
- `for` name unresolved (`UnresolvedItem`) or not a platform
  (`ExpectedPlatform`) => error.
- Declared incoming ports you don't read still need `unused` (normal
  module rules).

## Platform defaults

- `default <EXPR>` is legal ONLY on `outgoing` ports of a `platform`
  definition. On a module's own ports it is a semantic error
  (`DefaultOutsidePlatformPort`: "`default` on outgoing port oops is
  only allowed in a platform definition").
- `<EXPR>` is a constant expression (literal, enum variant, `@Invalid()`,
  `@Valid(n)`, struct literal, or compositions).
- When a module bound by `for` omits a defaulted `outgoing` port, a
  synthetic port is created and driven by the default expression —
  indistinguishable from a hand-written port (type-checked, documented).
  The default's type is checked against the port type; a mismatch is
  reported at the platform's expression.
- The module may instead declare and drive the port itself. Driving it
  WITHOUT declaring it is an error (`DriveUndeclaredPlatformPort`).
- A defaulted `incoming` port the module omits is synthesized as a
  never-read input and suppresses the unused-signal warning. There is no
  `default` on `incoming` ports (they cannot be driven from the module).
- A platform `outgoing` port WITHOUT `default` keeps the old behavior:
  omitting it is a missing-port error.

```virdant
platform SpiFlash {
    incoming clock     : Clock
    outgoing read_data : Valid[Word[32]]

    outgoing device_ready : Bit  default true
    outgoing spi_cs       : Bit  default true

    incoming spi_miso : Bit
}

// spi_cs and device_ready come from the platform defaults; only the
// ports you actually implement need to appear.
pub mod MySpiFlash for SpiFlash {
    incoming clock     : Clock
    outgoing read_data : Valid[Word[32]]
    incoming spi_miso  : Bit

    read_data := @Valid(0)
    unused clock
    unused spi_miso
}
```

## `@noinitial`

- Bare `@noinitial` on a `platform` forbids `initial` statements.
- When the top module is bound to such a platform, `vir check` reports
  `InitialNoinitialPlatform` for every `initial` in the top AND in every
  module reachable from it through the hierarchy (the diagnostic names
  the instantiation chain, e.g. `top::Top top::Sub top::Leaf`).
- Modules NOT reachable from a `@noinitial` top keep their `initial`s.

```virdant
@noinitial
platform StrictBoard {
    @pin(35) incoming clock : Clock
    @pin(39) outgoing led   : Bit
}
```

## Platform annotations

Item level:

- `@fpga("...")` — FPGA family; selects the toolchain at build time
  (only `ice40` and `xilinx` are supported today; an unsupported family
  passes `vir check` and fails in the build flow).
- `@part("...")` — part/package string passed to the place-and-route
  tool. ice40: `"hx1k-tq144"` becomes `--hx1k --package tq144`; xilinx:
  the full part string (`xc7a35tcsg324-1`) goes verbatim to
  nextpnr-xilinx, fasm2frames, xc7frames2bit.
- `@noinitial` — see above.

Port level (annotation goes BEFORE the declaration it annotates):

- `@pin(35)` — numeric pin; `@pin("A1")` — named pin/ball.
- `@period_ns(83)` or `@period_ns("83.33")` — clock period in
  nanoseconds, on a `Clock`-typed incoming port.

```virdant
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
```

## Shipped platforms

- `lib/ice40.vir` defines the iCE40 boards: `IceSugar`, `IceStick`,
  `Fomu` (plus `@cell` ext mods for the iCE40 hard cells).
- `lib/xilinx.vir` defines the Arty A7 boards: `ArtyA7`, `ArtyA7_100T`.
- Every `lib/*.vir` auto-loads; `import ice40` (etc.) puts its items in
  scope.
- `vir upload` infers the programmer from the platform name:
  `IceSugar` => `icesprog`, `IceStick` => `iceprog`, `Fomu` =>
  `dfu-util`, `ArtyA7`/`ArtyA7_100T` => `openFPGALoader`.

## Build flow

- FLOWS chain (`vir --help` "Flows"): `vir build` (Verilog only) =>
  `vir synth` (adds yosys) => `vir pnr` (place-and-route, reuses the
  synthesized netlist) => `vir bitstream` (full flow) => `vir upload`
  (bitstream + flash). Details: ./tools.md.
- `vir bitstream` requires `[prog] top` in `virdant.toml`; the top must
  be a concrete (non-generic) module with a `for` clause. It
  re-verifies the port match before invoking any external tool.
- `vir pnr` and `vir upload` accept `--tries N --seed S
  --allow-timing-failure`: nextpnr runs up to `N` times with seeds
  `S, S+1, ...`; the first attempt that closes timing wins. Per-seed
  attempt outputs are kept in `build/nextpnr/`.

## Gotchas

- A platform-synthesized port does NOT count toward the implicit-clock
  rule (only components the module declares count). If you omit the
  platform's clock port, declare `incoming clock : Clock` yourself if
  the module uses `reg`/`outgoing reg`.
- Pin constraints and clocks are emitted from `@pin`/`@period_ns`
  annotations — a port without `@pin` is unchecked until the build flow
  generates constraints.
