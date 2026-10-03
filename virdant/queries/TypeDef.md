# Query: TypeDef

## Summary

The builder `crate::types::typedef::build_typedef` (`typedef`) looks
up the `Symbol` for `symbol_id`, maps its `SymbolKind` to a `TypeScheme`, and for
`EnumDef` evaluates each enumerant's constant expression (the same logic as
`build_typedefs`, applied to a single type). Returns `Arc::new(TypeDef { ... })`.

## Signature

```rust
TypeDef(symbol_id: SymbolId) -> Arc<TypeDef>
```

## Result

A single user-defined type definition looked up by its `SymbolId`.

### `TypeDef`

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDef {
    pub symbol_id: SymbolId,
    pub kind: TypeScheme,

    pub width: Option<Width>,
    pub enumerant_values: IndexMap<SymbolId, WordValue>,
}
```

Defined in `virdant/src/types/typedef`. See the `TypeDefs` query report for
full field documentation.

- `symbol_id` - the `SymbolId` of this type definition.
- `kind` - a `TypeScheme` (`BuiltinDef`/`UnionDef`/`StructDef`/`EnumDef`).
- `width` - `Some(width)` for `EnumDef` (the first enumerant's literal width),
  `None` otherwise. `Width = u16`.
- `enumerant_values` - map from enumerant `SymbolId` to evaluated constant
  (`WordValue = u64`); empty for non-enum kinds.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)

## Example

For `tests/pass/gcd/src/top.vir`:

```virdant
union type State {
    Idle()
    Running(x : Word[8], y : Word[8])
    Done(result : Word[8])
}
```

`TypeDef(<State's SymbolId>)` returns a `TypeDef` with
`kind = TypeScheme::UnionDef`, `width = None`, and empty `enumerant_values`
(unions don't have enumerant values - only enums do). For the `Opcode` enum in
`tests/pass/typedefs/src/top.vir`, `TypeDef(<Opcode id>)` returns
`kind = EnumDef`, `width = Some(4)`, and `enumerant_values` mapping each
enumerant symbol to its integer literal value.
