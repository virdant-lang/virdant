# tests/golden — golden-master tests

Golden-master tests assert that generated Verilog is byte-identical to a
hand-expanded reference. This is a different verification layer from
`check/` (exit code + diagnostics) and `sim/` (behavioral simulation), and
is the recommended home for future platform-codegen goldens.

- `generics/` — a generic module and a hand-expanded copy of it must produce
  identical Verilog bodies. The Makefile checks, builds, greps for the
  escaped FQN, and tail-diffs the two files (dropping the header line).

Each golden test needs its own bespoke Makefile — see `generics/Makefile`
for the pattern.
