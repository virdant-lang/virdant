# tests/check — focused positive compile-time tests

Files here must compile cleanly: `vir check` must exit 0, and the harness
additionally greps the output for `WARNING` — a single unexpected warning is
a test failure. This is the suite that holds "this language feature works"
regressions that don't need diagnostics asserted.

Tests are directories named after the feature they exercise (operators,
generics, dynamic indexing, type inference, cycles, registers, when/match,
and other language corners). `projects/` contains project-style tests
(each with its own `Virdant.toml` and `src/`); see that directory's
README.

Some tests stay in `check/` rather than `warnings/` on purpose: their
assertion is that NO warning is emitted, and the check-suite's
`grep WARNING` is the mechanism that enforces it.

To add a test, create a directory (`check/X/`) holding `src/X.vir`, a
`Virdant.toml`, and a `test.toml` with `kind = "check"` and
`expect = "ok_no_warnings"`; the runner picks it up automatically.
