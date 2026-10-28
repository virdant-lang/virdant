# tests/format — formatter tests

Each test is a directory holding a pair: `NAME.vir` (input) and
`NAME.golden.vir` (expected formatted output), plus a `test.toml` with
`kind = "format"` naming both files. The runner copies the input to a
scratch dir, runs `vir format` on it, diffs against the golden file, and
then verifies the formatted output parses cleanly.

The parse gate greps the parse output for `Syntax Error` rather than
trusting `vir parse`'s exit code, because `vir parse` currently exits 0 even
on syntax errors (see TESTING_REPORT/NOTES.md; fixing the exit code is a
compiler change that needs separate approval).

To add a formatter test, create a directory with both files of the pair
and a `test.toml` (`input = "NAME.vir"`, `golden = "NAME.golden.vir"`);
the runner picks it up automatically.
