# Garden Guidelines
If you find any violations, write them to a CORRECTION_PROMPT.md
(or similarly named file),
a markdown document I can feed to an agent to correct each issue.
Separate each issue with Markdown `---` separators.

Never comment on what "looks good".
Only report positive violations.

# Garden Guidelines
The following is a list of rules and heuristics to be observed during code review.
Please report each violation of any of these rules.

## Language Changes
- Ensure that any changes to the language are reflected in `docs/source/reference`
- Ensure that any changes to the language are reflected in `agents/skills/virdant`

## Tool Changes
- When adding or changing tools, make sure the changes are reflected in `docs/source/tools`

## Code Comments
- Disallow unnecessary comments or overly verbose
- Disallow comments which are not written for human readability.
- Disallow comments which refer to a plan, a section of a plan,
  a question during a prompt, etc

## Strings and Resolution
- In the `virdant` crate, heavily discourage use of `String` or `&str`
  unless there is a VERY good reason not to use `BString` or `BStr`.
- Disallow using string manipulation when a `resolve_*` method would suffice.

## Filenames
- Disallow the use of the Rust filename `mod.rs`.
  Instead, require that it be named as `foo.rs` (for a module `foo`)
- Disallow the use of `-`'s inside of filenames and folder names.

## Rust
- Forbid `cargo c` to emit warnings.
- Forbid the suppression of warnings unless there is a decision to do so.
- Forbid using `.drain` on Vecs unless it is ever used again.
- Forbid fully-qualified names unless there is an established pattern already.
- When working with AST nodes, forbid grabbing number children (eg, `node.child(0)`)
  when it would be more appropriate to use or define
  a semantic helper in the node for that child (eg, `node.subject()`)

## Versioning
- Require that any change which introduces or modifies version is reconciled against `RELEASES.md`
- Require that any change to the version of any Rust, Python, or other package matches `VERSION`.
