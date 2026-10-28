# projects/patterns

Intended to test pattern-matching features (ident patterns and constructor
patterns with sub-patterns). This project is currently SKIPPED by its
Makefile: the source hits real pattern-matching issues (a false-positive
"Redundant else arm" on the skill-documented `case @Pair(true, false)` +
`else` example, and ident patterns reported non-exhaustive on Word
subjects). It was a `sleep 0` stub before the restructure, so it silently
did nothing; now it prints a SKIP notice pointing at
TESTING_REPORT/NOTES.md. Remove the skip once the pattern matcher is fixed.
