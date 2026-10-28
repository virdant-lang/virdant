# tests/check/projects — project-style positive tests

Each subdirectory is a full Virdant project: `Virdant.toml`, `src/`, and a
`test.toml` (`kind = "check"`, `expect = "ok"`); the runner runs `vir check`
in project mode. Discovery is automatic, so a new project is wired in
without editing any list.

Subdirectories group tests by feature area (annotations, parameters,
patterns) and by platform target (the default platform, specific boards, and
multi-platform `for`-clause layouts). See each subdirectory's own README for
its scope and current operational status.

To add a project test, create a new subdirectory following the existing
layout (plus a `test.toml`); the runner's automatic discovery handles the
wiring.
