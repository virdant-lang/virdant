# Query: ExprRootFor

## Summary

The builder `crate::queries::typecheck::build_exprroot_for`
(`queries/typecheck`) uses the private helper `find_exprroot`
(`typecheck`), which:

1. Gathers all `ExprRoot` IDs in the same package as `location`.
2. Starts at the node at `location.ast_node_id()` and walks **up** the AST
   (following `node.parent()`) until it reaches a node whose id is in the
   expression-root set, returning `Some(ExprRoot::new(node.location()))`.
3. If none is found, the builder dumps debug info (the node, its summary, the
   source text, its region) and panics with `"No ExprRoot found"`.

This is the bridge from an arbitrary expression `Location` back to the
`ExprRoot` that governs its typing, enabling `Typeof(location)` to find the
right `Typing` result.

## Signature

```rust
ExprRootFor(location: Location) -> ExprRoot
```

## Result

The `ExprRoot` that contains a given `Location` - i.e. walking up the AST from
the given node until reaching an expression root.

### `ExprRoot`

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    pub location: Location,
}
```

Defined in `virdant/src/types/typing` (see `ExprRoots` for full
documentation). A single-field struct wrapping a `Location`, marking an
expression root.

## Dependencies

* [`Parsing`](Parsing.md)
* [`ExprRoots`](ExprRoots.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
out := !last && inp
```

Given the `Location` of the `last` reference (a leaf inside `!last && inp`),
`ExprRootFor` walks up to the `!last && inp` node (the `Driver` RHS, which is an
expression root) and returns an `ExprRoot` whose `location` points at it.
`Typeof` then uses that `ExprRoot` to fetch the cached `Typing` and look up
`last`'s inferred type.
