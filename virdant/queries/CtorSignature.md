# Query: CtorSignature

## Summary

The builder `crate::types::signature::build_ctor_signature`
(`signature`):

1. Looks up the constructor symbol via `symboltable.symbol(ctor_symbol_id)`.
2. Sets `ret_typ = Type::Usual(parent_id)` where `parent_id` is the
   constructor symbol's parent (the containing `UnionDef` symbol id),
   `.unwrap()`-ing that it is `Some`.
3. Navigates to the constructor's AST node via `get_parsing` + `ast_node`.
4. Fetches the pre-built `TypeIndex` to resolve each parameter's type without
   re-running inference.
5. For each child whose payload is `AstNodePayload::Param`, reads the param's
   name and resolves the type from `param_node.child(1)` via
   `type_index.type_at(type_node.location())`, `.expect()`-ing it to be present.
6. Returns `Arc::new(Signature { parameters, ret_typ })`.

`CtorSignature` is consumed during `build_typing` by
`extend_context_with_stmt_match_pat` (`typing`) to type pattern-bound
variables in union-constructor patterns (e.g. `case @Running(x, y) => ...` binds
`x` and `y` to the parameter types).

## Signature

```rust
CtorSignature(ctor_symbol_id: SymbolId) -> Arc<Signature>
```

## Result

The type signature of a union constructor: its named parameters and return
type.

### `Signature`

```rust
/// The signature of a union constructor: its named arguments and return type.
#[derive(Debug, Clone)]
pub struct Signature {
    pub parameters: Vec<(BString, Type)>,
    pub ret_typ: Type,
}
```

Defined in `virdant/src/types/signature`.

- `parameters` - the ordered list of named parameters, each `(name: BString,
  typ: Type)`. Order matches the order of `Param` children in the constructor's
  AST node. Types are resolved via the `TypeIndex`.
- `ret_typ` - the return type. For a union constructor this is always
  `Type::Usual(parent_id)` where `parent_id` is the `SymbolId` of the containing
  `UnionDef`.

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
* [`TypeIndex`](TypeIndex.md)

## Example

For `tests/pass/gcd/src/top.vir`:

```virdant
union type State {
    Idle()
    Running(x : Word[8], y : Word[8])
    Done(result : Word[8])
}
```

`CtorSignature(<Running's ctor SymbolId>)` returns a `Signature` with
`parameters = [("x", Type::Word(8)), ("y", Type::Word(8))]` and
`ret_typ = Type::Usual(<State's SymbolId>)`. `CtorSignature(<Idle id>)` returns
`parameters = []` and the same `ret_typ`. `CtorSignature(<Done id>)` returns
`parameters = [("result", Type::Word(8))]`.
