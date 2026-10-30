# tests/golden — golden-master tests

Golden-master tests assert that generated Verilog is byte-identical to a
hand-expanded reference. This is a different verification layer from
`check/` (exit code + diagnostics) and `sim/` (behavioral simulation), and
is the recommended home for future platform-codegen goldens.

Subdirectories group tests by feature area (e.g. generics). Each golden
test's `test.toml` is bespoke — the typical pattern is a `kind =
"golden"` entry that checks, builds, greps for the escaped FQN, and
tail-diffs the generated Verilog against the reference via the `grep`
and `diff` keys (see `vir/src/virtue/README.md`).

To add a golden test, create a new subdirectory with the source, the
hand-expanded reference, and a `test.toml` following the existing
pattern.
