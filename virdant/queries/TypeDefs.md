# Query: TypeDefs

## Summary

The builder `crate::types::typedef::build_typedefs` (`typedef`) gets the
`SymbolTable`, calls `symboltable.typedefs()` to get all item-symbols that are
type definitions, and for each:

1. Maps the `SymbolKind` to a `TypeScheme` (`UnionDef`/`StructDef`/`EnumDef`/
   `BuiltinDef`; anything else is `unreachable!()`).
2. For `EnumDef` only, looks up the enum's AST node and iterates each
   `Enumerant` child, resolving the enumerant symbol via
   `symboltable.slot(item_symbol.id(), enumerant_name)`, evaluating the
   constant expression (`enumerant_node.child(1)`) via `eval_const_expr`
   (`typedef`), which handles `ExprWordLit`, `ExprParen`, and
   `ExprAs`. Word literals are parsed by `parse_word_literal` (which splits on
   `'w'` to obtain a value and optional width); numeric literals are parsed by
   `parse_nat_literal` (which supports `0x` hex, `0b` binary, and decimal,
   stripping `_` separators).
3. Records the first-seen width into `width` (later widths are ignored) and
   inserts `enumerant_id -> value` into `enumerant_values`.
4. For non-enum kinds, `width = None` and `enumerant_values` is empty.

The result is wrapped in `Arc`. `TypeDef` (the single-item query) shares this
logic but for one `symbol_id`.

## Signature

```rust
TypeDefs() -> Arc<Vec<TypeDef>>
```

## Result

Every user-defined type definition in the program, as a `Vec<TypeDef>`.

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

Defined in `virdant/src/types/typedef`.

- `symbol_id` - the `SymbolId` of this type definition in the global symbol table.
- `kind` - the classification. `TypeScheme` (`common`) is:
  ```rust
  pub enum TypeScheme {
      BuiltinDef,
      UnionDef,
      StructDef,
      EnumDef,
  }
  ```
- `width` - the inferred bit-width of an enum's enumerant values. `Some(width)`
  only for `EnumDef`, taken from the width suffix of the first enumerant's
  literal (e.g. `0w8` -> `Some(8)`); `None` for non-enum kinds and for unsized
  enum literals (e.g. `0` -> `None`). `Width` is `pub type Width = u16`.
- `enumerant_values` - an insertion-ordered map from each enumerant's
  `SymbolId` to its evaluated constant value. `WordValue` is `pub type WordValue =
  u64`. Empty for non-enum kinds.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)

## Example

For `tests/pass/typedefs/src/top.vir`:

```virdant
enum type Opcode {
    Add = 1w4
    Sub = 2
    And = 4
    Or  = 8
}
```

`TypeDefs()` includes a `TypeDef` for `Opcode` with `kind = EnumDef`,
`width = Some(4)` (from the `1w4` literal), and `enumerant_values` mapping
the `Add` symbol to `1`, `Sub` to `2`, `And` to `4`, `Or` to `8`. The
`Color` struct:

```virdant
struct type Color {
    red : Word[8]
    green : Word[8]
    blue : Word[8]
}
```

yields a `TypeDef` with `kind = StructDef`, `width = None`, and empty
`enumerant_values`.
