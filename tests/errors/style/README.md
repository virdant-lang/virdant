# style

Style-lint diagnostics that are errors, but not type errors:

- `when_match_style.vir` — the grammar accepts both `=> <expr>` arms and
  `{ ... }` block arms in both statement and expression position; mixing
  the two styles in the same construct is flagged.
