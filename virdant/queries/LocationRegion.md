# Query: LocationRegion

## Summary

The builder `crate::queries::build_location_region` (`queries/syntax`)
fetches the `Parsing` for `location.package()`, gets the `AstNode` at
`location.ast_node_id()`, and returns `node.region()` - which builds a `Region`
from the node's `Span` and the parsing's package. This is how the analysis layer
turns logical node references back into human-readable source positions for
diagnostics.

## Signature

```rust
LocationRegion(location: Location) -> Region
```

## Result

Resolves an AST-node `Location` to the source-text `Region` it occupies.

### `Location`

The canonical cross-package handle for a single AST node.

```rust
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Location(PackageFqn, AstNodeId);
```

Defined in `virdant/src/analysis/location`. Both fields are private;
accessors are `package()` and `ast_node_id()`. An `AstNodeId(pub u16)`
(`syntax/ast`) is only unique within one `Parsing`, so the `(package, id)`
pair is what makes a `Location` globally unique. It derives `Clone + Eq + Hash`,
so it is usable as a map key (and is, pervasively: `TypeAt`, `Typeof`,
`ExprRootFor`, `ComponentAnalysis::references`, etc.).

### `Region`

A source-text span, tagged with its package.

```rust
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Region {
    package: PackageFqn,
    span: Span,
}
```

Defined in `virdant/src/common/source`. `Span(LineCol, LineCol)`
(`source`) is a start/end pair of 1-indexed `LineCol(usize, usize)`s.
Accessors: `package()`, `span()`, `start()`, `end()`. `Display` renders as
`pkg[L:C-C]` (single line) or `pkg[L:C-L:C]` (multi-line).

The key distinction: a `Location` identifies a *node in the tree*, while a
`Region` identifies a *substring of source text*. `LocationRegion` bridges
from the former to the latter.

## Dependencies

* [`Parsing`](Parsing.md)

## Example

For `tests/pass/gcd/src/top.vir`, the `Driver` node `result := it.result` has a
`Location` like `Location("gcd", 204)`. `LocationRegion` of that `Location`
yields a `Region` such as `gcd[27:5-27:24]` - the byte range of that line -
which a diagnostic would display to the user.
