# Project Summary
* This is the repository for Virdant, a modern Hardware Description Language

# Codebase organization
* Documentation for agents (ie, for you) are in `agents/`
* Documentation for humans are in `docs/`
* Examples of Virdant code can be found in `tests/` and `tutorial/`.

# Tips
* Load the Virdant skill if you ever have to work with Virdant code.
* To test everything, use `make test`
* After changing `virdant.lalrpop`, run `make grammar` to regenerate `GRAMMAR.txt`
* NEVER run rustfmt (Rust), black (Python), or any other automatic code formatter without ASKING first.

# Versions
The `VERSION` file in the base directory is the canonical version of Virdant

Keep the following files in sync:
- virdant/Cargo.toml
- vir/Cargo.toml
- docs/pyproject.toml
