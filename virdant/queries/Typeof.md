# Query: Typeof

## Summary

The builder `crate::queries::build_typeof` (`queries/typecheck`):

1. Calls `find_exprroot(builder, location)` to walk up the AST to the enclosing
   `ExprRoot`. If none is found, returns `Err` with a single `Todo` diagnostic
   (`"No expr root"`).
2. Fetches the cached `Typing` for that `ExprRoot` via `get_typing`.
3. Looks up `typing.type_of_node(location.ast_node_id())`. If present, returns
   `Ok(typ.clone())`; otherwise `Err` with a `Todo` diagnostic
   (`"No expr root"`).

This is the user-facing way to ask "what type does this expression have?"
It delegates to `Typing`, which does the actual inference. The error path
returns diagnostics rather than panicking (unlike `ExprRootFor`).

## Signature

```rust
Typeof(location: Location) -> Result<Type, Vec<Diagnostic>>
```

## Result

The inferred `Type` of the expression at a given `Location`, or an error if no
expression root or no type annotation is found.

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

* [`Parsing`](Parsing.md)
* [`ExprRoots`](ExprRoots.md)
* [`Typing`](Typing.md)
* [`LocationRegion`](LocationRegion.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
out := !last && inp
```

`Typeof(<location of the `!last && inp` node>)` finds the enclosing `ExprRoot`,
fetches its `Typing`, and returns `Ok(Type::Bit)` (the expression is a bitwise
AND of two `Bit`s). `Typeof(<location of the `last` reference>)` returns
`Ok(Type::Bit)` as well. `Typeof(<location of a type-annotation node>)` would
return `Err` (no `ExprRoot` encloses a type annotation - those are handled by
`TypeAt`).
