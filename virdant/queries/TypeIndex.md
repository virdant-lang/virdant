# Query: TypeIndex

## Summary

The builder `crate::types::typedef::build_type_index` (`typedef`)
initializes an empty `TypeIndex`, fetches the `SymbolTable` and `Packages`, and
for each package calls `type_index.gather_type_roots(builder, parsing.root(),
&symboltable)` (`typedef`).

`gather_type_roots` is a recursive AST walker. When it encounters an
`AstNodePayload::Type` node, it:

1. Reads the child-0 `Ofness` to build a `type_name: BString` (optionally
   qualified with `package::`).
2. Resolves the name via `symboltable.resolve_item(type_name, parsing.package())`.
3. If unresolved, pushes an `UnresolvedType` and returns.
4. Otherwise, checks if the resolved symbol is one of the builtins
   (`builtin::Bit`, `builtin::Clock`, `builtin::Reset`, `builtin::Word`,
   `builtin::Valid`):
   - `Bit`, `Clock`, `Reset` take no parameters; extra children push an
     `Unknown` and return.
   - `Word` requires a `GenericsParams` child carrying a numeric value, parsed
     into a `Width` (`u16`), producing `Type::Word(width)`.
   - `Valid` requires a `GenericsType` child; it recurses into the inner type
     node first (so its `Type` gets indexed), reads the inner type via
     `self.type_at(inner_location)`, and produces `Type::Valid(Box::new(inner))`.
5. Otherwise the symbol is a user-defined type: `Type::Usual(symbol.id())`.
6. Deduplicates the type into `typs` (via `typ_to_index`, a linear scan) and
   inserts into `typ_at_location`.
7. It does **not** recurse into children of `Type` nodes (except the `Valid`
   inner-type special case), since `Ofness`/`GenericsParams`/etc. are not
   themselves `Type` nodes.

For non-`Type` nodes, it recurses into all children.

## Signature

```rust
TypeIndex() -> Arc<TypeIndex>
```

## Result

A global, location-keyed index of every type-annotation AST node in the parsed
sources, with each `Type` deduplicated into a shared set.

### `TypeIndex`

```rust
#[derive(Debug)]
pub struct TypeIndex {
    typs: IndexSet<Type>,
    typ_at_location: IndexMap<Location, TypeId>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct TypeId(usize);
```

Defined in `virdant/src/types/typedef`. All fields are private;
access is via methods.

- `typs` - the deduplicated, insertion-ordered set of all `Type`s encountered.
  Each distinct `Type` value appears once. Accessed via `typs() ->
  &IndexSet<Type>`.
- `typ_at_location` - maps each `Location` of a `Type` AST node to its
  `TypeId` (an index into `typs`). Accessed via
  `type_at(location) -> Option<&Type>` and `type_id_at(location) ->
  Option<TypeId>`.
- `diagnostics` - diagnostics collected during indexing (`Unknown` for
  malformed type applications like `Bit` taking parameters, `Word` missing a
  width, `Valid` missing a type; `UnresolvedType` when a type name cannot be
  resolved). Returned (cloned) via `diagnostics() -> Vec<Diagnostic>`.

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
* [`Packages`](Packages.md)
* [`Parsing`](Parsing.md)

## Example

For `tests/pass/gcd/src/top.vir`:

```virdant
union type State {
    Idle()
    Running(x : Word[8], y : Word[8])
    Done(result : Word[8])
}

mod Gcd {
    incoming clock : Clock
    incoming x : Word[8]
    ...
}
```

`TypeIndex()` indexes every type annotation in the file: `Clock` (at the
`clock` port's type node), `Word(8)` (at `x`, `y`, and `result` - all deduped to
one `TypeId`), and `State`/`Usual(...)` where referenced. `type_at(<location of
the `Word[8]` node>)` returns `Some(&Type::Word(8))`. `CtorSignature` uses this
to resolve constructor parameter types without re-running inference.
