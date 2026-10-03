# Query: TypeAt

## Summary

The builder `crate::types::typing::build_type_at` (`types/typing`)
fetches the `TypeIndex` and looks up `type_index.type_at(location)`. If present,
it returns `Ok(typ.clone())`; otherwise `Err(vec![])` (with a TODO noting that
diagnostics may not actually be needed here).

This is distinct from `Typeof`, which infers the type of an *expression* node
via the full inference engine. `TypeAt` resolves only *type-annotation* nodes
(nodes whose payload is `AstNodePayload::Type`), which the `TypeIndex` indexes
during its global walk.

## Signature

```rust
TypeAt(location: Location) -> Result<Type, Vec<Diagnostic>>
```

## Result

The `Type` of the type-annotation AST node at a given `Location`, looked up in
the global `TypeIndex`. This is used to resolve type annotations on
components, struct fields, and constructor parameters.

### `Type`

The typechecker's resolved type representation.

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

Defined in `virdant/src/types/typ`. `Width` is `u16`; `SymbolId` is
`pub struct SymbolId(pub u32)`.

- `Bit` - a single-bit logic value.
- `Clock` - a clock signal.
- `Reset` - a reset signal.
- `Word(Width)` - a word of the given bit-width.
- `Usual(SymbolId)` - a user-defined type (union/struct/enum/builtin)
  identified by its `SymbolId`. The `// TODO rename this` flags intent to
  rename.
- `Valid(Box<Type>)` - the builtin `Valid[T]` wrapper (a valid/invalid tag
  carrying an inner type). Recursive via `Box`.

## Dependencies

* [`TypeIndex`](TypeIndex.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
incoming clock : Clock
incoming inp   : Bit
out := !last && inp
```

`TypeAt(<location of the `Clock` type node>)` returns `Ok(Type::Clock)`.
`TypeAt(<location of the `Bit` type node>)` returns `Ok(Type::Bit)`. The
component builder calls `get_type_at` on the type-annotation child of each
`Component` statement to fill in `Component.typ`.
