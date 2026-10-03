# Query: MatchCoverage

## Summary

The builder `crate::queries::build_match_coverage`
(`queries/match_coverage`) works per-module:

1. Fetches the `Symbol`, its `AstNodeId`, the `Parsing`, and the item AST node.
2. **Early-out**: if `item_node.contains_errors()`, returns empty diagnostics
   immediately (no point checking coverage on broken syntax).
3. `collect_match_nodes` recursively walks the AST, recording
   every `ExprMatch` and `ModDefStmtMatch` node id (both expression-level and
   statement-level matches).
4. For each match node, takes `children[0]` as the subject, calls
   `builder.get_typeof(subject.location())`; on `Ok(typ)`, calls
   `check_match_coverage(builder, &node, &subject_typ, &mut diagnostics)`.
   On `Err`, the match is skipped.
5. Returns `Arc::new(diagnostics)`.

The actual coverage logic lives in `crate::types::match_coverage::check_match_coverage`
(`types/match_coverage.rs`). It performs:

- A structural pass detecting multiple `else` arms (`MatchMultipleElse`) and
  `else` not last (`MatchElseNotLast`).
- Pattern exhaustiveness and overlap checking against the subject type,
  emitting `MatchNonExhaustive` (some case not covered) and
  `MatchUnreachableArm` (an arm that can never fire because earlier arms already
  cover it) diagnostics.

## Signature

```rust
MatchCoverage(symbol_id: SymbolId) -> Arc<Vec<Diagnostic>>
```

## Result

Per-module match-coverage diagnostics: non-exhaustive matches, unreachable
arms, and structural `else` errors. There is no `MatchCoverage` data
structure; the query returns a `Vec<Diagnostic>` of match-coverage errors.

The return type is `Arc<Vec<Diagnostic>>` (see `SyntaxErrors` for the
`Diagnostic` definition).

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`SymbolAst`](SymbolAst.md)
* [`Parsing`](Parsing.md)
* [`Typeof`](Typeof.md)

## Example

For `tests/pass/gcd/src/top.vir`'s `Gcd` module:

```virdant
it <= match it : State {
    case @Idle() => mux(fire, @Running(x, y), @Idle())
    case @Running(x, y) =>
        mux(y == 0, @Done(x),
        mux(x < y, @Running(y - x, x),
        @Running(x - y, y)))
    case @Done(result) => @Idle()
}
```

`MatchCoverage(<Gcd id>)` collects this match node, infers the subject type as
`State` (a union), and checks that every constructor (`Idle`, `Running`, `Done`)
is covered by a `case`. All three are covered, so no `MatchNonExhaustive` is
emitted. An `else` arm is not present, so no `MatchMultipleElse` or
`MatchElseNotLast`. The result is an empty `Vec` for this exhaustive match.

A match that omitted `case @Done(result)` would produce
`MatchNonExhaustive { region, ... }` pointing at the match expression.
