# Project Summary
* This is the repository for Virdant, a modern Hardware Description Language

# Codebase organization
* Documentation for agents (ie, for you) are in `agents/`
* Documentation for humans are in `docs/`
* Examples of Virdant code can be found in `tests/` and `assets/tutorial/`.

# Tests
* To test everything, use `make test`
* When testing fails, check the log files in `build/tests/`
* When a test runs a simulation (`vir run` or `iverilog`),
  place an upper bound on the number of cycles,
  and when possible, set a tight limit

# Tips
* Load the Virdant skill if you ever have to work with Virdant code.
* `vir` CLI commands are either baked into the `vir` binary,
  or else `vir ${FOO}` is resolved to `vir-${FOO}`
* After changing `virdant.lalrpop`, run `make grammar` to regenerate `GRAMMAR.txt`
* NEVER run rustfmt (Rust) or any other automatic code formatter without ASKING first.

# Versions
The `VERSION` file in the base directory is the canonical version of Virdant

Keep the versions in following files in sync:
- virdant/Cargo.toml
- vir/Cargo.toml
- docs/pyproject.toml
