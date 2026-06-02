# Project Summary
* This is the repository for Virdant, a modern Hardware Description Language

# Codebase organization
* Documentation for agents (ie, for you) are in `agents/`
* Documentation for humans are in `docs/`
* Examples of Virdant code can be found in `examples/` and `tests/`
    (although the tests in `tests/warnings` and `tests/errors` show BAD examples of Virdant).

# Tips
* To test everything, use `make test`
* After changing `virdant.lalrpop`, run `make grammar` to regenerate `GRAMMAR.txt`
* In comments and in .md docs, use one sentence per line.
    And limit to 100 characters, breaking close to 80, especially on commas or relative clauses.
* Do NOT write unicode ever in code or docs. ONLY use ASCII.
* NEVER run rustfmt (Rust), black (Python), or any other automatic code formatter without ASKING first.
* Files must end in EXACTLY one newline at the end.
* Whenever you read a file, consider reading the associated `AGENTS.md` file in that directory (if present).
