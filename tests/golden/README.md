# tests/golden — golden-master tests

Golden-master tests assert that generated Verilog is byte-identical to a
hand-expanded reference. This is a different verification layer from
`check/` (exit code + diagnostics) and `sim/` (behavioral simulation), and
is the recommended home for future platform-codegen goldens.

Subdirectories group tests by feature area (e.g. generics). Each golden
test needs its own bespoke Makefile — the typical pattern is to check,
build, grep for the escaped FQN, and tail-diff the generated Verilog
against the reference (dropping the header line).

To add a golden test, create a new subdirectory with the source, the
hand-expanded reference, and a Makefile following the existing pattern.
