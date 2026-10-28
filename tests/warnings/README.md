# tests/warnings — warning-assertion tests

Every file in this suite must compile with exit code 0, but must produce the
warnings asserted by its `// CHECK:` directives. Unlike `errors/`, a
nonzero exit is the failure; unlike `check/`, warnings are expected. The
Makefile's flat loop asserts the exit code, and `filecheck` asserts the
warning text.

Files (one diagnostic family per file):

- `unused_source.vir` — the UnusedSource family: unused ports, unused
  outgoing wires, unused submodule outgoing ports (including the
  `unused`-statement escape hatch). Merged from the old `warnings.vir`
  UnusedSource cases plus `unused_submodule_port.vir`.
- `read_from_sink.vir` — the ReadFromSink family: reading from an outgoing
  port that is a sink. Split out of the old `warnings.vir`.
- `unfilled_hole.vir` — the UnfilledHole family: the `?` hole expression
  leaves a driver unimplemented. Split out of the old `warnings.vir`.
- `empty_driver_block.vir` — driver blocks with no drivers warn.
- `not_it.vir`, `not_it_initial.vir` — the NotIt family: using a
  component's own name instead of `it` inside its driver block, in
  statement and `initial` position (the old `initial_not_it.vir` was
  renamed).
- `no_reg_drivers.vir` — registers with no drivers warn.
- `redundant_driver.vir` — RedundantDriver warnings.
- `trailing_spaces.vir` — trailing whitespace on source lines.
- `platform_ports_no_unused.vir` — ports synthesized from platform
  defaults must not trigger UnusedSource warnings (renamed from the old
  `platform_default.vir`).

Each file ends with a single `// CHECK: Check OK` trailer. The suite
Makefile stays flat — it is deliberately simple and exits 0 when warnings
match.
