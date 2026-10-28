# tests/check — focused positive compile-time tests

Files here must compile cleanly: `vir check` must exit 0, and the harness
additionally greps the output for `WARNING` — a single unexpected warning is
a test failure. This is the suite that holds "this language feature works"
regressions that don't need diagnostics asserted.

The flat `.vir` files each exercise one feature:

- `alu.vir` — a small ALU: operators, bit-slicing, enums.
- `asclock.vir` — clock handling.
- `depends_on.vir` — `dependson` dependencies.
- `enum_onehot.vir` — one-hot enum encoding defaults.
- `generics_types.vir` — generic type-defs (renamed from the old
  `pass/generics.vir`).
- `generics_instances.vir` — generic module/socket instantiation.
- `index_dyn.vir` — dynamic indexing (`x[dyn e]`, renamed from
  `index_dyn_pass.vir`).
- `infer_binop.vir`, `infer_mux.vir` — inferring types from either side of
  binary operators and muxes.
- `module_cycle_negative.vir` — module instantiation graph that is NOT a
  cycle (renamed from `mod_cycle_no.vir`).
- `outgoing_reg.vir` — outgoing registers compile without warnings.
- `pub_kinds.vir` — the `pub` kinds on items.
- `reg_initial.vir` — register `initial` statements.
- `when_match_guards.vir` — when/match guards compile without warnings.
- `whens.vir` — `when` statements and expressions.

`projects/` contains project-style tests (each with its own `Virdant.toml`,
`src/`, and Makefile); see that directory's README.

`outgoing_reg.vir` and `when_match_guards.vir` stay in `check/` rather than
`warnings/` on purpose: their assertion is that NO warning is emitted, and
the check-suite's `grep WARNING` is the mechanism that enforces it.
