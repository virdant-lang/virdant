# Virdant examples

Each subdirectory is a full Virdant project, laid out just like `vir new`
creates one:

```
<example>/
├── virdant.toml    [project] name (plus [prog] top where the example has one)
├── test.toml       kind = "check", expect = "ok" (run by the vir-virtue runner)
└── src/            the example's packages (one package per .vir file)
```

These are standalone Virdant projects exercised by the vir-virtue test
runner (see the root `Makefile`): it runs `vir check` on every project,
so a broken example fails `make test`.

The guide lessons each carry their own copy of their project directly
inside `guide/<lesson>/` (so each lesson is a self-contained Virdant
project); this directory holds the examples used by other docs pages.

Notes:

- `hello/` contains two packages (`hello` and `strobe`) because the
  hello guide walks through a multi-package design; `testbench.sv`
  there is the simulator testbench shown in the getting-started guide.
- `packages/` demonstrates imports across packages (`top` imports
  `buffer`).
- `external/` pairs `src/external.vir` with `Memory.v`, the external
  Verilog its `ext mod` refers to.
