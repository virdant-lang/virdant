You are maintaining the query dependency documentation in `virdant/queries/`.

# Goal
Keep `*.md` query docs and `dependencies.dot` accurate against the Rust source in `virdant/src/`.

# Source of truth
Each query's builder function in `virdant/src/queries/` (e.g. `crate::queries::check`).
A query's **immediate dependencies** are the other queries it invokes via `Builder` getters
(`builder.get_xxx(...)`, `builder.typecheck(...)`, etc.).
Method calls on returned values are NOT query deps.

# When source changes
1. Find the builder fn (named in the query's `.md` Summary, e.g. `crate::queries::check`).
2. Trace ALL query-getter calls inside it, including via same-module helpers it calls (e.g. `item_for`, `find_exprroot`, `infer`, `check`). Do NOT skip helpers.
3. Compare against the `## Dependencies` list in the query's `.md`.
4. The `.md` lists **immediate** deps only, never transitive. Add missing; remove non-immediate.
5. Note: `Typing` and `Typeof` invoke each other (a cycle); deps leaving that cycle must ALL be kept verbatim in the `.dot`.

# After editing .md files
Rebuild `dependencies.dot` so it matches the union of all `.md` dependency lists:
- Edges: `A -> B` for every B in A's `## Dependencies`.
- Apply transitive reduction (see `make` / the reduction logic) to drop edges implied by longer paths
- Keep the existing cluster/grouping, layout attributes, and node declarations.
- The input queries have no dependencies.

# Verification
Run `make` (builds `dependencies.pdf` from `dependencies.dot`).

# Conventions
- `.md` dependency lines use the form: `* [`Name`](Name.md)`.
