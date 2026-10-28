# tests/warnings — warning-assertion tests

Every file in this suite must compile with exit code 0, but must produce the
warnings asserted by its `// CHECK:` directives. Unlike `errors/`, a
nonzero exit is the failure; unlike `check/`, warnings are expected. The
Makefile's flat loop asserts the exit code, and `filecheck` asserts the
warning text.

Files are organized one diagnostic family per file; the family is described
by the filename (unused sources, read-from-sink, unfilled holes, driver
blocks, not-`it` usage, missing register drivers, redundant drivers,
trailing whitespace, platform port synthesis, and other warning families).
Each file ends with a single `// CHECK: Check OK` trailer. The suite
Makefile stays flat — it is deliberately simple and exits 0 when warnings
match.

To add a warning test, create a new `.vir` file with the `// CHECK:`
directives for the warning you expect; the Makefile picks it up
automatically.
