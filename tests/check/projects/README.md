# tests/check/projects — project-style positive tests

Each subdirectory is a full Virdant project: `Virdant.toml`, `src/`, and its
own `Makefile` that runs `vir check` (project mode). The suite Makefile
recurses into every subdirectory automatically, so a new project is wired in
without editing any list.

- `annotations/` — annotations parse on various items and slots.
- `parameters/` — `@parameter` annotations.
- `patterns/` — pattern features (`Ident` bindings, constructor patterns
  with sub-patterns). Currently SKIPPED with a notice: the source predates
  the pattern-matcher's current behavior and does not check cleanly. See
  TESTING_REPORT/NOTES.md; it was a `sleep 0` stub before the restructure,
  so wiring it as an explicit skip is already an improvement.
- `platform_default/` — a project targeting the default platform.
- `platform_fomu/` — placeholder for a Fomu-target project; has no `src/`
  yet and is SKIPPED with a notice (it was never implemented; it was not
  wired into the old harness either).
- `platform_for_clause/` — `for`-clause modules across two platforms in one
  project with multiple files and instantiations. Its Virdant.toml was
  missing the `[project]` section header before the restructure.
- `platform_artya7/`, `platform_icestick/`, `platform_icesugar/` —
  board-target projects; these only run `vir check`, no hardware or
  synthesis is required.
