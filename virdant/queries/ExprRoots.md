# Query: ExprRoots

## Summary

The builder `crate::queries::find_exprroots` (`queries/syntax`) walks
every package's `PackageAnalysis` and collects the AST node IDs that
`PackageAnalysis::expr_roots_node_ids()` designates as expression roots, wrapping
each in `ExprRoot::new(Location::new(analysis.package(), ast_node_id))`. The
result is `Arc<Vec<ExprRoot>>`.

Expression roots are collected during `PackageAnalysis::add_item_expr_roots`
(see the `PackageAnalysis` report): driver RHS expressions, `when`/`match`
guards and subjects, enumerant values, and register clock expressions. Items
containing syntax errors are skipped.

## Signature

```rust
ExprRoots() -> Arc<Vec<ExprRoot>>
```

## Result

Every expression root in the program - the entry points the typechecker
processes.

### `ExprRoot`

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    pub location: Location,
}
```

Defined in `virdant/src/types/typing`. A single-field struct wrapping a
`Location`, marking an AST node that is the root of an expression to be
type-checked or inferred (e.g. a driver RHS, a `when` guard, a `match` subject,
an enumerant value, a register clock expression).

Methods: `new(location)`, `location() -> Location` (clones), and
`package() -> PackageFqn` (delegates to `location.package()`). Because it
derives `Eq + Hash`, it is usable as a query key (used by `ExpectedType` and
`Typing`).

## Dependencies

* [`Packages`](Packages.md)
* [`PackageAnalysis`](PackageAnalysis.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
out := !last && inp
```

The `!last && inp` expression node is an expression root (the RHS of a
continuous driver). `ExprRoots()` includes an `ExprRoot` whose `location`
points at that node. Likewise, the `it <= inp` inside the `reg` block is
another root. `Typing(exprroot)` then type-checks that whole expression subtree.
