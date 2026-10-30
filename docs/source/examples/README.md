# Virdant examples

Each subdirectory is a full Virdant project, laid out just like `vir new`
creates one:

```
<example>/
├── virdant.toml    [project] name (plus [prog] top where the example has one)
├── test.toml       kind = "check", expect = "ok" (run by the vir-virtue runner)
└── src/            the example's packages (one package per .vir file)
```

These projects double as the live sources for the documentation: the
tutorial pages `literalinclude` files from here, and the vir-virtue test
runner (see the root `Makefile`) runs `vir check` on every project, so a
broken example fails `make test`.

Notes:

- `hello/` contains two packages (`hello` and `strobe`) because the
  hello tutorial walks through a multi-package design; `testbench.sv`
  there is the simulator testbench shown in the getting-started guide.
- `packages/` demonstrates imports across packages (`top` imports
  `buffer`).
- `external/` pairs `src/external.vir` with `Memory.v`, the external
  Verilog its `ext mod` refers to.
