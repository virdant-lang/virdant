---
name: commit
description:
    Guidance for writing commit messages in this repository.
    Use when preparing, drafting, or reviewing a git commit.
---

# Commit

## Instructions

- Commit all *staged* changes.
- However, NEVER stage changes yourself

## TL;DR

```
PREFIX: Imperative summary ending with a period.

One or more wrapped paragraphs explaining what changed and why,
using `backticks` for code identifiers. ~72 columns wide.
```

## 1. Use a `PREFIX:` tag

Every commit in the recent history begins with an uppercase `PREFIX:` tag
that classifies the *kind* of change. Pick the most specific tag that fits.

| Prefix        | Use for                                                                 |
| ------------- | ----------------------------------------------------------------------- |
| `FEATURE`     | New user-facing functionality or language features.                     |
| `FIX`         | A bug fix. (`BUGFIX:` is an accepted synonym — both appear in history.) |
| `REFACTOR`    | Restructuring that does not change behavior.                            |
| `CHANGE`      | A modification that alters behavior or an API (semantic change).        |
| `DIAGNOSTICS` | New warnings/errors or changes to diagnostic messages.                  |
| `DOCS`        | Documentation (prose, tutorials, rustdocs, mdbook).                     |
| `VIRDOC`      | The `vir doc` documentation-generation tooling specifically.            |
| `TEST`        | Adding or changing tests. (`TESTS:` is an accepted synonym.)            |
| `EXAMPLES`    | Example `.vir` programs.                                                 |
| `GRAMMAR`     | `virdant.lalrpop` grammar changes (regenerate `GRAMMAR.txt` too).        |
| `BUILTIN`     | The `builtin` module / standard definitions.                           |
| `DB`          | The salsa database or `vir db` tooling.                                 |
| `CLEANUP`     | Removing dead code, tidying, no behavior change.                        |
| `WORKFLOW`    | CI/CD, GitHub workflows, release scripts.                              |
| `RELEASE`     | Version bumps and release cuts.                                        |
| `REPO`        | Repo-level config (`.gitignore`, `AGENTS.md`, top-level meta).         |
| `ARCHITECTURE`| `ARCHITECTURE.md` and high-level design docs.                           |
| `ASSETS`      | Logos, images, and other binary assets.                                |
| `SKILL`       | The Virdant agent skill (`agents/skills/virdant/`).                    |
| `AGENTS`      | `AGENTS.md` and other agent-facing docs.                               |

If a commit genuinely spans multiple categories, lead with the **most
significant** one rather than stacking prefixes.

## 2. Subject line

- **Imperative mood**, as if completing the sentence "If applied, this
  commit will ___": `Add`, `Fix`, `Remove`, `Rename`, `Merge`, `Replace`,
  `Suppress`, `Bump`, etc.
- Capitalize the first word after the prefix.
- **End with a period.** This is the dominant recent style (~70% of recent
  commits; older history is noisier).
- Keep it under ~72 characters when reasonable. The historical median is 36
  characters; only a small minority exceed 72. If you cannot stay short,
  prefer a concise subject and put detail in the body.
- Wrap code identifiers in backticks when they aid clarity:
  `FIX: Support \`ExprAs\` in constant expression evaluation.`

## 3. Body (optional but encouraged for non-trivial changes)

Separate the body from the subject with a **blank line**. Only a minority of
commits include a body (~19% of recent commits), but it is expected for
non-trivial changes — especially refactors that touch multiple subsystems —
where the subject alone cannot convey the "why".

- Wrap paragraphs at **~80 columns**.
- Use `` `backticks` `` for type names, function names, modules, and other
  code identifiers — heavily used in this repo's bodies.
- Describe **what** changed and, more importantly, **why**. Call out
  affected modules, call-site counts, and behavioral consequences.
- Bullet lists (using `-`) are common when enumerating several points.
- There is no convention in this repo for `Co-authored-by`,
  `Signed-off-by`, or issue-closing trailers; do not add them unless a
  specific need arises.

## 4. Examples

A small, self-contained change:

```
DOCS: Minor wording tweak to tutorial.
```

A focused refactor with a body:

```
REFACTOR: Merge import validation into a single pass.

Combines `add_imports` and `validate_imports` in `PackageAnalysis`
into one `add_and_validate_imports` method that inserts and checks
package existence in a single traversal, avoiding a redundant pass.

Rewrites `SymbolTable::resolve_item_in_package` to iterate symbols
directly by package and name instead of formatting and looking up a
string FQN.
```

A feature that touches several subsystems (lead with the primary category):

```
REFACTOR: Replace PackageFqn with dense PackageId throughout the compiler.

Removes the `PackageFqn`/`ItemFqn` types, keeping only `PackageId` and
`PackageTable` in the `package` module. `Location`, `Region`, `Source`,
`InternedString`, and the db queries are now keyed by `PackageId`, and
several of these become `Copy`, dropping their clones.

Name resolution (`resolve_item`/`resolve_item_in_package`, ofness
handling in elaboration, cycle and symbol-table building) now resolves
strings to `PackageId`s via `PackageTable::id`, returning `None`
instead of silently falling back when a package is absent.
```

## 5. Things to avoid

- **No prefix.** Early history has many bare "Add ..."/"Fix ..." subjects;
  the current convention requires a `PREFIX:` tag.
- **Lowercase prefixes** like `docs:` or `sim:`. These exist in the middle
  of the history but are not used anymore.
- **Vague subjects** such as `WIP`, `More patterns.`, `Fixes.` — say what
  actually changed.
- **Redundant prefixes** like `BUGFIX: Fix ...` (the prefix already says
  "fix"). Prefer `BUGFIX: Drop ... ` / `BUGFIX: Handle ...`.
- **Exceeding ~72 columns** in the subject. Move the detail to the body.
- **Forgetting the trailing period** on the subject.
