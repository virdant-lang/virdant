# tests/warnings — warning-assertion tests

Every test in this suite must compile with exit code 0, but must produce the
warnings asserted by its `// CHECK:` directives. Unlike `errors/`, a
nonzero exit is the failure; unlike `check/`, warnings are expected. The
runner asserts the exit code (`expect = "ok"`), and `filecheck` asserts the
warning text.

Files are organized one diagnostic family per file; the family is described
by the filename (unused sources, read-from-sink, unfilled holes, driver
blocks, not-`it` usage, missing register drivers, redundant drivers,
trailing whitespace, platform port synthesis, and other warning families).
Each file ends with a single `// CHECK: Check OK` trailer.

To add a warning test, create a directory (`warnings/X/`) with
`src/X.vir` (carrying the `// CHECK:` directives for the warning you
expect), a `virdant.toml`, and a `test.toml` with `kind = "check"` and
`expect = "ok"`; the runner picks it up automatically.
