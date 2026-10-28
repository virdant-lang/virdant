# virtue

The Virdant test runner (`virtue`): a pure orchestrator that runs every test as a
subprocess of `target/release/vir` (plus `iverilog` / `verilator` for the
sim suite), with the same commands and exit-code contracts the
per-directory Makefiles used before the runner replaced them. It links
nothing from the compiler.

Run from the repo root (or anywhere above it):

```
virtue                    run ALL tests, regardless of failures
virtue <test> ...         run only the named tests
virtue --suite sim        run one suite (top-level group of the test path)
virtue --filter substr    run tests whose name contains substr
virtue --list             list all tests
virtue -ff                --fail-fast: stop scheduling on first failure
virtue --jobs <n>         worker threads (default: min(cores, 16))
```

Exit code 0 iff no failures and no policy violations.

## Adding a test

Add a directory **anywhere in the project** containing a `test.toml`
(`tests/` is the convention). The test's name is its directory path
relative to the repo root (`tests/<path/to/test>`), used identically on
screen and under `build/tests/`. Nothing else needs wiring — discovery
walks the whole project automatically (build trees and hidden
directories are skipped).

Single-file language tests use a full project layout:

```
tests/errors/typing/word_literals/
├── Virdant.toml            [package] name = "word_literals"
├── src/word_literals.vir   the test source, with CHECK directives
└── test.toml               kind, contract, description
```

The source file is named after the test (not `top.vir`) so the package
name — and therefore the diagnostics quoted by CHECK directives — stays
the same as it was when the file was run with `vir -F`.

## `test.toml` format

Common keys:

| key           | meaning                                            |
|---------------|----------------------------------------------------|
| `kind`        | `check`, `sim`, `format`, `golden`, or `doc`       |
| `description` | one-liner shown next to the test name              |
| `skip`        | `true`, or a string reason; skipped tests show `⚠️` |

### `kind = "check"`

Runs `vir <command>` in the test directory, then (optionally) validates
the merged stdout+stderr against CHECK directives with `filecheck`.

| key        | meaning                                                          |
|------------|------------------------------------------------------------------|
| `command`  | `vir` subcommand (default `check`; e.g. `"bitstream"`)           |
| `expect`   | exit-code contract: `ok`, `ok_no_warnings`, `ok_with_warnings`, `error` |
| `filecheck`| check-file relative to the test dir (e.g. `src/top.vir`)         |

`filecheck` consumes stdout ++ stderr concatenated — byte-compatible
with the Makefiles' `> out 2>&1`.

### `kind = "sim"`

Runs `vir build`, then the simulator:

| key         | meaning                                                        |
|-------------|----------------------------------------------------------------|
| `simulator` | `iverilog` (default) or `verilator`                            |
| `sources`   | iverilog sources (default `["build/*.sv", "tb.sv"]`)           |
| `flags`     | simulator flags (e.g. `["-g2012"]`)                            |
| `lint_only` | verilator: pass `--lint-only` (default true)                   |

iverilog compiles to `build/run` and then runs `./build/run`.
The suite needs the simulators installed; if missing, the runner fails
loudly unless `VIRDANT_ALLOW_MISSING_SIM=1`, in which case sim tests are
skipped with a notice.

### `kind = "format"`

Formats a copy of `input`, compares it byte-for-byte against `golden`,
then checks the formatted file parses (`vir parse` output must not
contain `Syntax Error` — the parse gate greps, because `vir parse`
exits 0 even on syntax errors).

| key      | default       |
|----------|---------------|
| `input`  | `input.vir`   |
| `golden` | `golden.vir`  |

### `kind = "golden"`

Runs `vir check` and `vir build`, then:

- `grep` — list of `{ path, pattern }`: each file must contain the pattern
- `diff` — list of `{ a, b, skip_lines }`: files must be identical after
  skipping the first `skip_lines` lines of each

### `kind = "doc"`

Runs `vir doc`, then:

- `expect_files` — list of paths that must have been generated
- `grep` — list of `{ path, pattern }` content assertions

## Output

On a terminal, the display is a fixed viewport repainted in place at
the bottom of the screen (like `top`); the terminal never scrolls, so
nothing is pushed off the top and the scrollback is never written to.
The viewport holds one line per test

```
 ✅ tests/check/whens            whens
 ❌ tests/errors/typing/word_literals  ...
 ⌛ tests/sim/fifo               ...      (animated in place)
 ⚠️ tests/check/projects/platform_fomu  ... (reason)
```

— a window over the canonical test list ending at the dispatch
frontier (every test, when the terminal is tall enough) — blank rows
reserved for the summary, and a progress line (`⌛ 43/116 done, 15
running`) on the last row.

Descriptions are padded two spaces past the longest test name and cut
at the terminal edge with `...`.

At the end the summary is printed into the viewport without clearing
or scrolling: skips and failures are listed in the reserved rows, the
counts line (`116 tests: 114 passed, ...`) replaces the progress line,
and the board with every test's final status stays on screen above.

Without a terminal (piped output), each test's line is printed as
soon as that test finishes — no in-progress lines, no escape codes —
followed by the full summary.  In piped mode SIGINT keeps its default
behavior; on a terminal, Ctrl+C clears the display's bottom lines and
parks the cursor at the start of the next line (exit code 130).

## Evidence

A failing test writes `build/tests/<path/to/test>/test.log`: the exact
command line, exit codes, merged output, filecheck stderr, duration,
and status. Written iff the test failed.

## FileCheck mode

The FileCheck matcher moved here from the `virdant` crate:

```
virtue --filecheck <check-file>   # directives in <check-file>, input on stdin
```

A `filecheck` compatibility binary is also built, for anyone invoking
the matcher directly (it is also what `make install` installs).
