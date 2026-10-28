# tests/check/projects — project-style positive tests

Each subdirectory is a full Virdant project: `Virdant.toml`, `src/`, and its
own `Makefile` that runs `vir check` (project mode). The suite Makefile
recurses into every subdirectory automatically, so a new project is wired in
without editing any list.

Subdirectories group tests by feature area (annotations, parameters,
patterns) and by platform target (the default platform, specific boards, and
multi-platform `for`-clause layouts). See each subdirectory's own README for
its scope and current operational status.

To add a project test, create a new subdirectory following the existing
layout; the Makefile recursion handles the wiring.
