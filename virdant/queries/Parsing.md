# Query: Parsing

## Summary

The builder `crate::queries::build_parsing` (`queries/parsing`) fetches
the input `Source` for the package and calls `parse(&source)`
(`syntax/parsing`). That tokenizes the text, runs the LALRPOP
`PackageParser`, then fixes up the `parents` array by replaying the post-order
node stream through a stack. The result is wrapped in `Arc` and
memoized.

`Parsing` is the substrate every analysis query reads: `PackageAnalysis` walks
`parsing.root().children()`, `ComponentAnalysis` resolves paths through
`parsing.ast_node(...)`, and `SyntaxErrors` extracts `parsing.diagnostics()`.

## Signature

```rust
Parsing(package: PackageFqn) -> Arc<Parsing>
```

## Result

The complete parse output of a single package: the arena-style AST, the
interned-string table, and the parse diagnostics, all bundled into one
`Parsing`.

### `Parsing`

```rust
#[derive(Debug)]
pub struct Parsing {
    pub(super) source: Source,
    pub(super) strings: Vec<BString>,
    pub(super) payloads: Vec<AstNodePayload>,
    pub(super) spans: Vec<Span>,
    pub(super) parents: Vec<AstNodeId>,
    pub(super) num_children: Vec<u16>,
    pub(super) errors: Vec<AstNodeId>,
    pub(super) error_data: Vec<ParseError>,
    pub(super) docstring_diagnostics: Vec<Diagnostic>,
}
```

Defined in `virdant/src/syntax/parsing`. All fields are `pub(super)`,
visible only within the `syntax` module; external access is via methods.

- `source` - the originating `Source` (carries the package name and text).
- `strings` - the interned string table. Identifiers and string literals are
  stored once here and referred to by `InternedString` handles.
- `payloads` - parallel array of node payloads, one entry per AST node.
  `AstNodePayload` is the tagged union of all node kinds.
- `spans` - parallel array of source `Span`s, one per node.
- `parents` - parallel array of parent `AstNodeId`s. The root's parent is
  itself (fixed up after the post-order parse stream is replayed).
- `num_children` - parallel array of child counts, used to reconstruct the tree
  from the post-order stream the LALRPOP parser produces.
- `errors` - IDs of `Error` payload nodes (sentinels for parse failures), in
  insertion order.
- `error_data` - the LALRPOP error records, zipped 1:1 with `errors`.
- `docstring_diagnostics` - diagnostics emitted while checking docstrings.

### Notable related types

- `InternedString` (`parsing`): a `{package, id}` handle into `strings`.
- `AstNodePayload` (`syntax/payload.rs`): the node-kind enum.
- `AstNodeId(pub u16)` and `AstNode<'a>` (`syntax/ast`): the node
  handle and the borrowed view (payload + parent + a `&Parsing`).
- `ParseError` (`parsing`): alias for
  `lalrpop_util::ErrorRecovery<SourceOffset, Token, TokenError>`.

### Notable methods

`package()`, `root() -> AstNode`, `ast_node(id) -> AstNode`,
`string(s: InternedString) -> &BStr`, `intern(span) -> InternedString`,
`errors()`, `diagnostics()`, `at(linecol) -> Option<AstNodeId>` (positional
lookup picking the innermost containing node), `dump()`.

## Dependencies

* [`Source`](Source.md)

## Example

For `tests/pass/edge/src/top.vir`, `Parsing("top")` produces a tree whose root
is the `Package` node with a single `ModDef` child `Top`. The `strings` table
interns `Top`, `clock`, `Clock`, `reset`, `Reset`, `inp`, `Bit`, `last`,
`out`, etc. `intern` dedupes so the two uses of `Clock` share one
`InternedString`. The `spans` array records each node's byte range; e.g. the
`reg last : Bit on clock` statement node has a span covering that whole line.
If the file had a syntax error, `errors`/`error_data` would record the recovery,
and `diagnostics()` would synthesize `SyntaxError` diagnostics from them.
