# tests/sim — behavioral end-to-end tests

Each subdirectory is a full Virdant project: `src/` Virdant sources,
`Virdant.toml`, a SystemVerilog testbench (`tb.sv`, or `fifo_tb.sv` for the
FIFO), and a Makefile that runs `vir build` and then simulates the generated
Verilog with iverilog (or verilator, for `uart/`). These are the repository's
behavioral tests — they exercise codegen lowering end to end, which no other
suite covers.

## Toolchain policy

- If iverilog/verilator are installed, every project runs as part of the
  default `make test`.
- If a simulator is missing, the suite FAILS loudly — unless
  `VIRDANT_ALLOW_MISSING_SIM=1` is set, in which case projects are skipped
  with a `SKIP:` notice instead. CI sets that variable after installing the
  tools; locally, `make test` runs everything when the tools are present.

## Projects

- `basics/` — core operators and literals checked through generated Verilog
  (bit/word ops, concatenation, ascription, truncation).
- `dontcare/` — dontcare values in simulation.
- `drivers/` — driver forms (`:=`, `<=`, it-blocks) around an enum state
  machine; the tb asserts all forms lower to the same values on every edge.
- `edge/` — edge detection (rising/falling).
- `extmod/` — `ext mod` declarations bound to an external SystemVerilog
  module (`ext/Rom.sv` with a `rom.hex` payload).
- `fifo/` — FIFO with randomized push/pop stress testing.
- `gcd/` — GCD computed by a union-type state machine, swept over values.
- `generics_mod/` — generic sockets and mods instantiated at concrete type
  arguments and wired end-to-end through `:=:` connections.
- `i2c_open_drain/` — open-drain I2C bus semantics.
- `lfsr/` — an LFSR.
- `literals/` — literal handling in simulation.
- `pipe/` — the builtin generic socket `Pipe[T]`, instantiated at two
  different type arguments in one top, connected with `:=:` on mono sockets.
- `sockets/` — client/server sockets between instances (core + memory).
- `statemachine/` — traffic-light state machine over an enum type.
- `submod_socket/` — regression for the socket-wiring bug where two
  submodules are connected with `:=:` inside an instantiation block
  (`m.busif :=: it.busif`); asserts the end-to-end value.
- `sync/` — the builtin `Sync` component syncing an async input.
- `typedefs/` — enum/struct typedefs in simulation.
- `uart/` — UART design; verilator lint-only (no testbench).
- `valid/` — Valid matching; asserts the `foo` wire produced by a
  `match @Valid(n)` (the test's one distinguishing feature).
- `when_guard/` — regression test for the "lost when-arm guard" bug.
