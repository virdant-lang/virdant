# tests/errors — must-fail compilation tests

Every `.vir` file in this suite must FAIL `vir check`. The harness captures
the compiler output and runs `filecheck` over the file's own
`// CHECK:` / `// CHECK-NOT:` directives to assert the exact diagnostics,
then verifies the exit code was nonzero. A file that compiles cleanly is a
test failure.

Files are grouped by diagnostic family in feature subdirectories that mirror
the diagnostic families in `diagnostics.rs`. Subdirectories recursed into by
the suite Makefile include syntax errors, name-resolution errors, type
errors (with a nested area for generics), driver-structure errors,
match-expression errors, cycle detection, `pub` visibility, platform
handling (project-style tests), and style lints. The source `.vir` files are
the authority on the exact coverage of each family.

## Conventions

- One file asserts one diagnostic family; flavors of the same family live in
  the same file.
- Each file ends with a single `// CHECK: Check failed` trailer.
- File names are the package name for single-file tests, so CHECK lines that
  quote package-qualified paths must be updated when a file is renamed.
- The suite Makefile recurses into subdirectories automatically; a new
  feature directory just needs the standard two-part Makefile
  (flat `VIR_FILES` loop + `SUBDIRS` recursion).

## Adding a test

Add a `.vir` file (or project directory with its own `Virdant.toml` and
Makefile) under the subdirectory matching its diagnostic family, with
FileCheck directives asserting the exact diagnostic text and the standard
`// CHECK: Check failed` trailer. No central list needs updating — the
Makefile recursion wires it in automatically.
