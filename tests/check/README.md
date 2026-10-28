# tests/check — focused positive compile-time tests

Files here must compile cleanly: `vir check` must exit 0, and the harness
additionally greps the output for `WARNING` — a single unexpected warning is
a test failure. This is the suite that holds "this language feature works"
regressions that don't need diagnostics asserted.

The flat `.vir` files each exercise one feature (operators, generics,
dynamic indexing, type inference, cycles, registers, when/match, and other
language corners), described by their filename. `projects/` contains
project-style tests (each with its own `Virdant.toml`, `src/`, and
Makefile); see that directory's README.

Some tests stay in `check/` rather than `warnings/` on purpose: their
assertion is that NO warning is emitted, and the check-suite's
`grep WARNING` is the mechanism that enforces it.

To add a test, drop a new `.vir` file in this directory; the Makefile
picks it up automatically.
