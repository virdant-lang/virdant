# tests/errors — must-fail compilation tests

Every `.vir` file in this suite must FAIL `vir check`. The harness captures
the compiler output and runs `filecheck` over the file's own
`// CHECK:` / `// CHECK-NOT:` directives to assert the exact diagnostics,
then verifies the exit code was nonzero. A file that compiles cleanly is a
test failure.

Tests are grouped by diagnostic family in feature subdirectories that mirror
the diagnostic families in `diagnostics.rs`: syntax errors, name-resolution errors, type
errors (with a nested area for generics), driver-structure errors,
match-expression errors, cycle detection, `pub` visibility, platform
handling (project-style tests), and style lints. The source `.vir` files are
the authority on the exact coverage of each family.

## Conventions

- One test asserts one diagnostic family; flavors of the same family live in
  the same file.
- Each file ends with a single `// CHECK: Check failed` trailer.
- The source file stem is the package name for single-file tests, so CHECK
  lines that quote package-qualified paths must be updated when a test
  directory is renamed.

## Adding a test

Add a directory under the subdirectory matching its diagnostic family
(`errors/<feature>/X/` with `src/X.vir`, a `virdant.toml`, and a
`test.toml` with `kind = "check"` and `expect = "error"`), or a project
directory with its own `virdant.toml` for multi-file tests. Add FileCheck
directives asserting the exact diagnostic text and the standard
`// CHECK: Check failed` trailer. No central list needs updating — the
runner discovers any directory containing a `test.toml`.
