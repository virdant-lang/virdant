# Query: SymbolAst

## Summary

The builder `crate::analysis::symbols::build_symbol_ast`
(`symbols`) simply looks up the `Symbol` for the given `SymbolId`
in the `SymbolTable` and returns the `AstNodeId` stored in its `Location`:

```rust
pub(crate) fn build_symbol_ast(builder: &mut Builder, symbol_id: SymbolId) -> AstNodeId {
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    symbol.location().ast_node_id()
}
```

> Note: the `db` declaration carries a TODO suggesting this could be
> replaced by a `Symbol(symbol_id) -> Arc<Symbol>` query returning the whole
> symbol (with its `Location`).

## Signature

```rust
SymbolAst(symbol_id: SymbolId) -> AstNodeId
```

## Result

The `AstNodeId` of the syntax-tree node that declared the given symbol. This is
the canonical way to go from a logical `SymbolId` back to the syntax tree.

### `AstNodeId`

```rust
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstNodeId(pub u16);
```

Defined in `virdant/src/syntax/ast`. A dense, per-`Parsing` index into the
parallel arrays in `Parsing` (`payloads`, `spans`, `parents`, `num_children`).
The `.index()` method returns `self.0 as usize`. Its `Debug` prints
`AstNodeId(<n>)`.

### `SymbolId`

```rust
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);
```

Defined in `virdant/src/analysis/symbols`. A dense index into
`SymbolTable::symbols_by_id`. `Debug` prints `SymbolId(<n>)`.

## Dependencies

* [`SymbolTable`](SymbolTable.md)

## Example

For `tests/pass/gcd/src/top.vir`, `SymbolTable()` assigns `State` the
`SymbolId` `7` (say). `SymbolAst(7)` returns the `AstNodeId` of the `UnionDef`
node for `State` - e.g. `AstNodeId(3)` - which is the index into `Parsing("gcd")`
's arrays where that node's payload, span, and parent live. From there
`parsing.ast_node(3)` yields the borrowed `AstNode` for the union definition.
