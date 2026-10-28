# projects/platform_for_clause

Positive test that the `for` clause works in a project with multiple files
and module instantiations across multiple platforms.

Before the restructure this project had no Makefile and a malformed
`Virdant.toml` (bare keys without the `[project]` header), and its source
referenced nonexistent items; all three were fixed and the project is now
wired in.
