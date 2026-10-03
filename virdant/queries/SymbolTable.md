# Query: SymbolTable

## Summary

The builder `crate::analysis::symbols::build_symboltable`
(`symbols`) iterates every package, fetches its `PackageAnalysis`,
and calls `build_symboltable_item` for each item. That function
builds a `Location`, an FQN `"package::item"`, a `SymbolKind` (via
`node_to_symbol_kind`), assigns a `SymbolId(symbols.len())`, and pushes the item
`Symbol`. If the package is `builtin`, it also records the name in
`builtin_names`. It then dispatches on the payload to add slot symbols:

- `ModDef` -> `build_symboltable_moddef_slot` creates
  `Component`, `Submodule`, `Socket` slot symbols with `parent_id: Some(item_id)`,
  emitting `DuplicateSlot` for repeats.
- `StructDef`/`UnionDef`/`EnumDef`/`BuiltinDef` ->
  `build_symboltable_typedef_slot` creates `Field`, `Ctor`,
  `Enumerant` slot symbols.

Slot FQNs are `"package::item::slot"` and slot `SymbolId`s are assigned
sequentially by the running `symbols.len()`. Finally, `symbols_by_id` is built
by collecting the values in insertion order, and the `SymbolTable` is wrapped in
`Arc`.

## Signature

```rust
SymbolTable() -> Arc<SymbolTable>
```

## Result

The global symbol table: every named declaration across every package, indexed
both by fully-qualified name and by dense `SymbolId`.

### `SymbolTable`

```rust
#[derive(Debug)]
pub struct SymbolTable {
    pub symbols: IndexMap<BString, Symbol>,
    symbols_by_id: Vec<Symbol>,
    pub diagnostics: Vec<Diagnostic>,
    pub builtin_names: IndexSet<BString>,
}
```

Defined in `virdant/src/analysis/symbols`.

- `symbols` - all named declarations keyed by fully-qualified name (e.g.
  `"pkg::Item"` or `"pkg::Item::slot"`). Insertion order is deterministic:
  packages in the order `Packages()` returns them, then item order, then slot
  order.
- `symbols_by_id` - the same symbols indexed by `SymbolId.0` for O(1) lookup
  by id, via `symbol(symbol_id)`.
- `diagnostics` - aggregated diagnostics from every `PackageAnalysis` plus
  `DuplicateSlot` diagnostics.
- `builtin_names` - item names declared in the `builtin` package, used by
  `resolve_item` to fall back to `builtin` when a name is otherwise
  unqualified.

### `Symbol` and `SymbolId`

```rust
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub fqn: BString,
    pub name: BString,
    pub location: Location,
    pub kind: SymbolKind,
    pub parent_id: Option<SymbolId>,
}
```

Defined in `virdant/src/analysis/symbols`.

- `id` - the dense id matching the index in `symbols_by_id`.
- `fqn` - fully-qualified name (`"pkg::Item"` for items, `"pkg::Item::slot"`
  for slots).
- `name` - the short, unqualified name.
- `location` - the `Location` of the declaring AST node.
- `kind` - a `SymbolKind` (see below).
- `parent_id` - `None` for top-level items; `Some(item_id)` for slots,
  pointing at the enclosing item.

### `SymbolKind`

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    ModDef,
    UnionDef,
    StructDef,
    EnumDef,
    BuiltinDef,
    FnDef,
    SocketDef,
    Component,
    Submodule,
    Socket,
    Field,
    Ctor,
    Enumerant,
}
```

Defined in `virdant/src/analysis/symbols`. `is_item()` is true for the
seven item kinds; `is_typedef()` is true for `UnionDef`/`StructDef`/
`EnumDef`/`BuiltinDef`.

## Dependencies

* [`Packages`](Packages.md)
* [`PackageAnalysis`](PackageAnalysis.md)
* [`Parsing`](Parsing.md)

## Example

For `tests/pass/gcd/src/top.vir`:

```virdant
union type State {
    Idle()
    Running(x : Word[8], y : Word[8])
    Done(result : Word[8])
}

export mod Top {
    ...
    mod gcd of Gcd { ... }
}

mod Gcd {
    incoming clock : Clock
    ...
}
```

`SymbolTable()` contains, among others:
- `Symbol { fqn: "gcd::State", kind: UnionDef, parent_id: None, ... }`
- `Symbol { fqn: "gcd::State::Idle", kind: Ctor, parent_id: <State id>, ... }`
- `Symbol { fqn: "gcd::State::Running", kind: Ctor, ... }` plus its parameter
  fields `x`, `y` are not separate symbols (they're ctor params, handled via
  `CtorSignature`).
- `Symbol { fqn: "gcd::Top", kind: ModDef, ... }`
- `Symbol { fqn: "gcd::Gcd", kind: ModDef, ... }`
- `Symbol { fqn: "gcd::Gcd::clock", kind: Component, parent_id: <Gcd id>, ... }`
- ...one `Component` symbol per port of `Gcd`.

`resolve_item("Gcd", "gcd")` returns the `Gcd` symbol; `slot(<Gcd id>, "clock")`
returns the `clock` component symbol.
