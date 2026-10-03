# Query: Typing

## Summary

The builder `crate::types::typing::build_typing` (`typing`):

1. Walks up the AST from the expr root's node until it finds the enclosing item
   node, reads its name, and resolves the containing item `Symbol` via
   `symboltable.resolve_item_in_package`.
2. Fetches the base `TypingContext` via `get_typing_context(item.id())`, then
   extends it with pattern-bound locals from enclosing `ModDefStmtMatch` arms
   via `extend_context_for_enclosing_stmt_matches`, which handles
   `PatIdent`, `PatCtor` with `Valid`/`Invalid`, and union-constructor patterns
   (looking up `CtorSignature` to bind payload variables to parameter types).
   Bindings are pushed outermost-first so innermost arms shadow.
3. Reads the `ExpectedType` via `get_expected_type(exprroot)`.
4. Driver-RHS special case: if `expected_typ.is_none()` and the
   parent is a `Driver`, checks whether the LHS component exists; if not, sets
   `skip_validate = true` to suppress downstream "Missing annotation" cascades
   (the RHS is still type-checked to catch unknown references).
5. Constructs the empty `Typing`.
6. If there is an expected type, calls `typing.check(builder, context, &node,
   &expected_typ)` (the private `check` submodule). Otherwise calls
   `typing.infer(builder, context, &node)` (the private `infer` submodule). If
   inference returns `Ok(None)` and the node is not a driver RHS, pushes a
   `Todo` diagnostic.
7. If `!skip_validate`, calls `typing.validate(builder, &parsing)` (lines
   190-210): a no-op if errors already exist or `has_unresolved_referent`;
   otherwise BFS-traverses the AST and flags any `node.is_expr()` node missing
   from `typs` with an `Unknown { "Missing annotation" }` diagnostic.
8. Returns `Arc::new(typing)`.

## Signature

```rust
Typing(exprroot: ExprRoot) -> Arc<Typing>
```

## Result

The central per-expression-root type-inference and checking result: type
annotations for every node in the expression, resolution tags, diagnostics, and
records of which components were referenced where.

### `Typing`

```rust
#[derive(Debug)]
pub struct Typing {
    item: Symbol,
    exprroot: ExprRoot,
    typs: IndexMap<AstNodeId, Type>,
    diagnostics: Vec<Diagnostic>,
    use_locations: IndexMap<BString, Vec<Location>>, // TODO should be use for Referents
    tags: IndexMap<Location, Tag>,
}
```

Defined in `virdant/src/types/typing`.

- `item` - the containing item (module/union/struct/etc.) this typing belongs to.
  A `Symbol` (see `SymbolTable`).
- `exprroot` - the `ExprRoot` this `Typing` was built for.
- `typs` - type annotations: a map from `AstNodeId` to its inferred/checked
  `Type`. Read via `type_of_node(id) -> Option<&Type>`. Populated by `annotate()`,
  which panics if a node is annotated twice.
- `diagnostics` - diagnostics emitted during checking/inference
  (`WrongType`, `Todo`, `Unknown`, `NotWordType`, `CantInfer`, etc.).
- `use_locations` - records, for each component path (`BString`) used in this
  expression, the `Location`s where it was referenced. Read via
  `reference_use_locations()`; added to via `use_component(path, location)`.
  Consumed by `TypeCheck` to aggregate uses for unused/read-from-sink warnings.
- `tags` - per-location resolution tags, recording what a given `Location`
  resolved to. Read via `tag(location) -> Tag` (returns `Tag::None` if absent).

### `Tag` and `Primitive`

```rust
#[derive(Debug, Clone)]
pub enum Tag {
    None,
    SymbolResolution(SymbolId),
    PrimitiveResolution(Primitive),
    ReferentResolution(Referent),
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Any,
    All,
    Cast,
    Word,
    Sext,
    Zext,
    Trunc,
    Mux,
}
```

Defined in `virdant/src/types/typing`.

- `Tag::None` - no resolution recorded.
- `Tag::SymbolResolution(SymbolId)` - the location resolved to a symbol
  (e.g. a type name).
- `Tag::PrimitiveResolution(Primitive)` - resolved to a built-in primitive
  operation (`Any`, `All`, `Cast`, `Word`, `Sext`, `Zext`, `Trunc`, `Mux`).
- `Tag::ReferentResolution(Referent)` - resolved to a `Referent` (Component or
  Local). Used in `has_unresolved_referent` to detect cascade-suppression
  cases.

## Dependencies

* [`Parsing`](Parsing.md)
* [`SymbolTable`](SymbolTable.md)
* [`TypingContext`](TypingContext.md)
* [`Typeof`](Typeof.md)
* [`CtorSignature`](CtorSignature.md)
* [`ExpectedType`](ExpectedType.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`Component`](Component.md)
* [`StructFields`](StructFields.md)
* [`TypeIndex`](TypeIndex.md)
* [`TypeDef`](TypeDef.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
out := !last && inp
```

`Typing(<ExprRoot for !last && inp>)` produces a `Typing` whose `typs` maps the
root node to `Type::Bit`, the `!last` node to `Bit`, the `last` reference to
`Bit`, the `inp` reference to `Bit`, and the `&&` node to `Bit`. Its `tags`
record that `last` and `inp` resolved to `ReferentResolution(Component(...))`.
`use_locations` records `last` and `inp` with their reference `Location`s. The
`diagnostics` vec is empty for this valid expression.
