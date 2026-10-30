# Virdant tests

This directory holds all Virdant compiler and tooling tests. The suites are
organized **first by verification layer** (what harness runs them and what
exit-code contract they have), **then by language feature** within a layer.

Run everything from the repo root with `make test` (= `cargo test` +
`target/release/vir-virtue` over every suite, including the docs example
projects under `docs/source/examples/`).  The
runner discovers every test automatically: a test is any directory in
the project containing a `test.toml` (the whole tree is searched;
`tests/` is the convention), and its name is its directory path
(`tests/<path/to/test>`).  See `vir/src/virtue/README.md` for the
`test.toml` format and the runner's CLI (`--list`, `--suite`,
`--filter`, `--jobs`, `-ff`).

## Suites

- `errors/` — programs that MUST fail to compile. Each `.vir` file is run
  through `vir check`; the test fails if the run succeeds, and FileCheck
  directives assert the exact diagnostics. Organized into feature
  subdirectories that group files by diagnostic family.
- `warnings/` — programs that compile with expected warnings. The test
  must exit 0; FileCheck directives assert which warnings
  appear (and which do not).
- `check/` — focused, positive compile-time tests. Files must compile with
  **no warnings at all** (the harness greps the output for `WARNING`).
  Flat `.vir` files exercise language features; `projects/` contains
  project-style tests (each with its own `Virdant.toml`).
- `sim/` — end-to-end behavioral tests: `vir build` + iverilog (or verilator)
  simulation of a testbench against the generated Verilog. Requires
  iverilog/verilator; see the toolchain policy below.
- `golden/` — golden-master tests: generated Verilog must be byte-identical
  to a hand-expanded reference.
- `format/` — formatter tests: each input `.vir` must format to exactly its
  `.golden.vir` counterpart, and the formatted output must parse cleanly.
- `doc/` — `vir doc` HTML-generation tests (file existence + docstring
  content assertions).

## Exit-code contracts

- `errors/`: `vir check` must exit nonzero, and every `CHECK:` directive must
  match. A file that passes `vir check` is a test failure.
- `warnings/`: `vir check` must exit 0 (warnings are OK); FileCheck asserts
  the warnings.
- `check/`: `vir check` must exit 0, no `WARNING` may appear in the output,
  and FileCheck directives must match.
- `sim/`, `golden/`, `format/`, `doc/`: the runner asserts output correctness
  directly (simulator exit codes, diffs, greps).

## FileCheck directive conventions

- A FileCheck directive is a comment line `// CHECK: <pattern>` matched
  against the compiler output of the file being tested.
- `// CHECK-NOT: <pattern>` asserts the pattern does NOT appear anywhere in
  the output.
- Errors-suite files end with `// CHECK: Check failed`; warnings/check suite
  files end with `// CHECK: Check OK`.
- One file asserts **one diagnostic family** (or one feature) — keep merges
  to related flavors of the same family.

## Layout of a test directory

Every test is a directory containing a `test.toml` (kind, one-line
description, optional `skip`).  Single-file language tests are full
projects named after the test:

```
tests/errors/typing/word_literals/
├── Virdant.toml            [package] name = "word_literals"
├── src/word_literals.vir   the test source, with CHECK directives
└── test.toml               kind = "check", expect, filecheck, description
```

Project tests (`check/projects/`, `sim/`, `errors/platforms/`, …) keep
their `Virdant.toml` + `src/` layout and get a `test.toml` describing
their kind (`check`, `sim`, …).

## Adding a test

- A new diagnostic for language construct X: add
  `errors/<feature>/X/` with `src/X.vir`, a `Virdant.toml`, and a
  `test.toml` (`kind = "check"`, `expect = "error"`) with CHECKs for
  the exact diagnostic text, plus a `// CHECK: Check failed` trailer.
- A regression that must compile cleanly: add `check/X/`
  (`expect = "ok_no_warnings"`), or a `check/projects/<name>/` project
  if it needs multiple files / a platform.
- A behavioral test: add `sim/<name>/` with `src/top.vir`, `tb.sv`,
  `Virdant.toml`, and a `test.toml` (`kind = "sim"`).
- A formatter change: add `format/<name>/` with the input `.vir` plus
  `<name>.golden.vir` (the golden file is the expected formatted
  output) and a `test.toml` (`kind = "format"`).
- Rename caution: the package name of a single-file test is its source
  file stem, so CHECK lines that quote package-qualified names (e.g.
  `Type foo::Bar takes 1 type argument`) must be updated when a test
  directory is renamed.

## Sim toolchain policy

`tests/sim/` needs `iverilog` (and `verilator` for `uart/`). If either is
missing, the runner fails loudly, unless the environment
variable `VIRDANT_ALLOW_MISSING_SIM=1` is set, in which case every project is
skipped with a notice. CI installs the tools and sets the variable; local
`make test` runs everything when the tools are present.

## Layout notes

- Test discovery is automatic: the runner recurses under `tests/` and
  treats any directory with a `test.toml` as a test — no central list
  to forget to update, and no Makefile to write.
- `build/` output directories are gitignored and excluded from
  discovery.
- Failing tests leave evidence in `build/tests/<path/to/test>/test.log`
  (exact command line, exit codes, merged output, filecheck stderr,
  duration, status).
- `TESTING_REPORT/NOTES.md` records the reasoning behind the current
  layout and any known, deliberately skipped tests.
