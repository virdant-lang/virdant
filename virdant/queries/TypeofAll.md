# Query: TypeofAll

## Summary

The builder `crate::queries::build_typeof_all` (`queries/typecheck`)
iterates every `Location` from `AllExprs()`, resolves the containing item, and
skips items whose root node `contains_errors()`. For each remaining expression,
it calls `get_typeof(location)`; on `Ok(typ)`, it inserts
`(location, Some(typ))` into the map. On `Err`, the entry is omitted (so the
`Location` simply does not appear).

This is a bulk fan-out over `Typeof`, useful for tooling that wants to know
every expression's type at once (e.g. an IDE or a documentation generator).

## Signature

```rust
TypeofAll() -> IndexMap<Location, Option<Type>>
```

## Result

A map from every expression `Location` in the program to its inferred `Type`
(or `None` if inference failed or the item had syntax errors).

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

The return type is an `IndexMap<Location, Option<Type>>` - insertion-ordered,
keyed by `Location`. `Some(typ)` means inference succeeded; `None` means the
item contained syntax errors (so the expression was skipped) or `Typeof`
returned `Err`.

## Dependencies

* [`AllExprs`](AllExprs.md)
* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)
* [`Typeof`](Typeof.md)

## Example

For `tests/pass/edge/src/top.vir`, `TypeofAll()` returns a map containing, among
others:

- `<location of !last && inp>` -> `Some(Type::Bit)`
- `<location of last>` -> `Some(Type::Bit)`
- `<location of inp>` -> `Some(Type::Bit)`
- `<location of it <= inp>` -> `Some(Type::Bit)` (the reg driver RHS)

Expressions inside items with syntax errors would be absent from the map
(skipped by the `contains_errors` guard).
