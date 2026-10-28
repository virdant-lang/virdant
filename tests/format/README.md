# tests/format — formatter tests

Each test is a pair: `NAME.vir` (input) and `NAME.golden.vir` (expected
formatted output). The Makefile copies the input to a temp dir, runs
`vir format` on it, diffs against the golden file, and then verifies the
formatted output parses cleanly.

The parse gate greps the parse output for `Syntax Error` rather than
trusting `vir parse`'s exit code, because `vir parse` currently exits 0 even
on syntax errors (see TESTING_REPORT/NOTES.md; fixing the exit code is a
compiler change that needs separate approval).

12 input/golden pairs cover indentation, alignment, continuations,
comments, it-blocks, initial statements, and assorted item kinds. To add a
formatter test, create both files of a pair; the Makefile picks them up
automatically.
