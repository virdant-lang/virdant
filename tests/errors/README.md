# tests/errors — must-fail compilation tests

Every `.vir` file in this suite must FAIL `vir check`. The harness captures
the compiler output and runs `filecheck` over the file's own
`// CHECK:` / `// CHECK-NOT:` directives to assert the exact diagnostics,
then verifies the exit code was nonzero. A file that compiles cleanly is a
test failure.

Files are organized into feature subdirectories that mirror the diagnostic
families in `diagnostics.rs`:

- `syntax/` — parser and lexer errors (syntax errors, tabs, indentation,
  docstring spacing).
- `naming/` — unresolved names and duplicate items (`unresolved.vir`,
  `imports.vir`, `duplicate_items.vir`).
- `typing/` — type-checking errors: driver type mismatches, builtin type
  arity, word literals, when-condition types, index bounds, inference
  failures, enum value rules (`enum_values.vir`), and a `generics/`
  subdirectory for generic-parameterization errors.
- `drivers/` — driver-structure errors: illegal driver targets, drive-sink
  violations, when-driver rules, missing `on` clauses, initial-statement
  errors, redundant/unused driver combinations.
- `match/` — match-expression errors: non-exhaustive matches per subject
  type (`exhaustive_*.vir`), overlapping arms (`overlap.vir`), misplaced or
  redundant `else` arms (`else_arms.vir`), and pattern-type mismatches.
- `cycles/` — combinational loops and module-definition cycles.
- `visibility/` — `pub` surface violations (`pub_leak.vir`,
  `pub_leak_surfaces.vir`, `pub_for_notpub.vir`) plus the `pub_notpub/`
  project-style test.
- `platforms/` — platform-related errors, as project-style tests:
  `platform_default/`, `platform_mismatch/`, and `top_generics/`.
- `style/` — style-lint errors that are diagnostics but not type errors
  (`when_match_style.vir`).

## Conventions

- One file asserts one diagnostic family; flavors of the same family live in
  the same file (e.g. all four match-overlap subject types are merged in
  `match/overlap.vir`).
- Each file ends with a single `// CHECK: Check failed` trailer.
- File names are the package name for single-file tests, so CHECK lines that
  quote package-qualified paths (e.g. `Type arity::Keep takes 1 type
  argument`) must be updated when a file is renamed.
- The suite Makefile recurses into subdirectories automatically; a new
  feature directory just needs the standard two-part Makefile
  (flat `VIR_FILES` loop + `SUBDIRS` recursion).
