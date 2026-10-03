# Query: StructFields

## Summary

The builder `crate::analysis::structs::build_struct_fields`
(`structs`) returns an empty `Vec` unless
`symbol.kind() == SymbolKind::StructDef`. Otherwise it gets the struct's
`Location`, fetches the `Parsing` and the struct AST node, and iterates the
**direct children** of the struct node. For each child whose payload is
`AstNodePayload::Field(field)`:

1. Reads `field.name` via `parsing.string(...)`.
2. Looks up the field symbol via `symboltable.slot(symbol_id, field_name)`; skips
   if `None`.
3. Resolves the type from the first child of the `Field` node
   (`field_node.child(1)`) via `builder.get_type_at(typ_node.location())`. `Ok(t)`
   becomes `Some(t)`; `Err(_)` becomes `None` (type-resolution failures do not
   abort the whole list).
4. Pushes `StructField { field_symbol_id, name, typ }`.

Returns the `Vec` directly (no `Arc` - it is cheap enough to clone).

## Signature

```rust
StructFields(symbol_id: SymbolId) -> Vec<StructField>
```

## Result

The list of fields declared by a struct type definition, each with its name,
field symbol id, and resolved type.

### `StructField`

```rust
#[derive(Debug, Clone)]
pub struct StructField {
    pub field_symbol_id: SymbolId, // TODO make these not pub
    pub name: BString,
    pub typ: Option<Type>,
}
```

Defined in `virdant/src/analysis/structs`.

- `field_symbol_id` - the `SymbolId` of the field's slot in the symbol table.
  (A `// TODO make these not pub` comment indicates the author intends to hide
  this behind an accessor later.)
- `name` - the field's name as a `BString`.
- `typ` - the resolved `Type` of the field's type annotation, or `None` if
  `get_type_at` returned `Err` (e.g. type-resolution failures don't abort the
  whole list).

Accessors (`structs`): `name() -> &BStr`, `typ() -> Option<&Type>`,
`symbol_id() -> SymbolId`.

### `Type`

```rust
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bit,
    Clock,
    Reset,
    Word(Width),
    Usual(SymbolId), // TODO rename this
    Valid(Box<Type>),
}
```

Defined in `virdant/src/types/typ` (see `TypeAt` for full documentation).

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)
* [`TypeAt`](TypeAt.md)

## Example

For `tests/pass/typedefs/src/top.vir`:

```virdant
struct type Color {
    red : Word[8]
    green : Word[8]
    blue : Word[8]
}
```

`StructFields(<Color's SymbolId>)` returns a `Vec<StructField>` containing:
- `StructField { name: "red", typ: Some(Type::Word(8)), ... }`
- `StructField { name: "green", typ: Some(Type::Word(8)), ... }`
- `StructField { name: "blue", typ: Some(Type::Word(8)), ... }`

Each `field_symbol_id` is the `SymbolId` of the corresponding field slot in the
`SymbolTable` (e.g. `"top::Color::red"`). The struct construction
`color := ${ red = 0, green = 128, blue = 255 }` and field access
`color->green` use this to check that the field names and types match.
