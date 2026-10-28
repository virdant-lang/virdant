# match

Match-expression errors, one family per file:

- `exhaustive_bit.vir`, `exhaustive_enum.vir`, `exhaustive_union.vir`,
  `exhaustive_word.vir` — MatchCoverage: non-exhaustive matches on Bit,
  enum, union, and Word subjects (renamed from
  `match_missing_*.vir` / `match_word_needs_else.vir`).
- `overlap.vir` — Overlapping match arms across all pattern flavors:
  identifiers, literals, unions, and dontcare (merged from
  `match_overlap.vir`, `match_overlap_bit.vir`, `match_overlap_union.vir`,
  `match_overlap_word.vir`).
- `else_arms.vir` — `else`-arm placement rules: not-last, multiple, and
  redundant else arms (merged from `match_else_not_last.vir`,
  `match_multiple_else.vir`, `match_redundant_else.vir`).
- `pattern_types.vir` — pattern/type mismatches (was
  `match_pat_type.vir`).
