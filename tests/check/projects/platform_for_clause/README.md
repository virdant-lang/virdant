# projects/platform_for_clause

Positive test that the `for` clause works in a project with multiple files
and module instantiations: two platforms (`PlatformA`, `PlatformB`), a
platform-specific module for each (`ModuleA for PlatformA`,
`ModuleB for PlatformB`), and a top-level module instantiating both.
Before the restructure this project had no Makefile and a malformed
`Virdant.toml` (bare keys without the `[project]` header), and its source
referenced nonexistent `FpgaModule`/`AsicModule` items; all three were
fixed and the project is now wired in.
