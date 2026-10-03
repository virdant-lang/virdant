# Query: TypingContext

## Summary

The builder `crate::queries::build_typing_context` (`queries/typing`)
fetches `ComponentAnalysis` for the given module, starts with an empty
`TypingContext::new()`, and for each `(path, component)` in
`component_analysis.components()` pushes a component binding via
`push_component(name, component_id, typ)`. The component's `typ()` is
`Option<Type>` (may be `None` if `get_type_at` failed during resolution).

Local bindings are added later, during `build_typing`, by
`extend_context_for_enclosing_stmt_matches` and
`extend_context_with_stmt_match_pat` (`typing`), which call
`push_local(name, location, typ)` for each pattern-bound variable.

## Signature

```rust
TypingContext(symbol_id: SymbolId) -> TypingContext
```

## Result

The scoped name-binding environment for a module: a stack of bindings mapping
identifiers to their referent (component or local) and optional type, used
during inference and checking to resolve names in scope.

### `TypingContext`

```rust
#[derive(Debug, Clone)]
pub struct TypingContext {
    context: Vec<(BString, (Referent, Option<Type>))>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Referent {
    Component(ComponentId),
    Local(Location),
}
```

Defined in `virdant/src/types/context`.

The single private field `context` is a `Vec` of bindings, innermost (most
recently pushed) at the end. `get` searches in reverse, so later
bindings shadow earlier ones.

Each binding is `(name: BString, (referent: Referent, typ: Option<Type>))`:

- `name` - the identifier spelling (e.g. a component path like `it.foo` or a
  pattern-bound variable name).
- `referent` - what the name refers to:
  - `Referent::Component(ComponentId)` - a resolved component
    (signal/port/register).
  - `Referent::Local(Location)` - a locally bound variable introduced by a
    pattern (e.g. `r` in `case @Done(r) => ...`).
- `typ` - the optional `Type` of the referent. For locals it is always `Some`;
  for components it may be `None` if the component's type failed to resolve.

`TypingContext` is returned by value (not `Arc`), because it is cheap to `Clone`
(a `Vec` of tuples).

## Dependencies

* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`TypeDefs`](TypeDefs.md)

## Example

For `tests/pass/gcd/src/top.vir`'s `Gcd` module:

```virdant
mod Gcd {
    incoming clock : Clock
    incoming reset : Bit
    ...
}
```

`TypingContext(<Gcd id>)` returns a context with one binding per component:
`("clock", Component(<clock id>), Some(Type::Clock))`,
`("reset", Component(<reset id>), Some(Type::Bit))`, etc. During `build_typing`
of a `match state { case @Done(r) => ... }` arm, the context is extended with
`("r", Local(<r node>), Some(Type::Word(8)))` so that the body `result := r`
can resolve `r`.
