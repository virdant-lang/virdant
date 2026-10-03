# Query: AllExprs

## Summary

The builder `crate::queries::build_all_exprs` (`queries/syntax`) walks
every package's `Parsing::root()` AST recursively, pushing `node.location()`
for every node where `node.is_expr()` is true. The result is
`Arc<Vec<Location>>`.

This enumerates *every* expression node (not just roots), so it is finer-grained
than `ExprRoots`. It is consumed by `TypeofAll`, which type-checks each
expression and records the result in a map keyed by `Location`.

## Signature

```rust
AllExprs() -> Arc<Vec<Location>>
```

## Result

The `Location` of every expression AST node in the program.

> Note: the `db` declaration carries `// TODO remove if possible`,
> indicating this query may be redundant with `ExprRoots` and `Typing`.

## Dependencies

* [`Packages`](Packages.md)
* [`Parsing`](Parsing.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
out := !last && inp
```

`AllExprs()` includes the `Location`s of the whole `!last && inp` expression,
the `!last` sub-expression, the `last` reference, the `inp` reference, and the
`&&` application - every expression node in the tree. `TypeofAll` then maps
each to its inferred `Type` (where inference succeeds).
