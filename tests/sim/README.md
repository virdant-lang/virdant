# tests/sim — behavioral end-to-end tests

Each subdirectory is a full Virdant project: `src/` Virdant sources,
`Virdant.toml`, a SystemVerilog testbench (`tb.sv`), and a `test.toml`
(`kind = "sim"`). The runner runs `vir build` and then simulates the
generated Verilog with iverilog (verilator for lint-only projects).
These are the repository's behavioral
tests — they exercise codegen lowering end to end, which no other suite
covers.

## Toolchain policy

- If iverilog/verilator are installed, every project runs as part of the
  default `make test`.
- If a simulator is missing, the suite FAILS loudly — unless
  `VIRDANT_ALLOW_MISSING_SIM=1` is set, in which case projects are skipped
  with a notice instead. CI sets that variable after installing the
  tools; locally, `make test` runs everything when the tools are present.

## Adding a new sim project

- Create a new subdirectory under `tests/sim/` with the standard layout:
  `src/`, `Virdant.toml`, a `tb.sv` testbench, and a `test.toml` with
  `kind = "sim"` (plus `simulator`/`flags`/`sources` when the defaults
  don't fit; see `vir/src/virtue/README.md`).
- The runner runs `vir build` and then simulates the generated Verilog.
- Put the assertions in the testbench (`tb.sv`); the runner only builds
  and runs the simulation.
