# Query: ExpectedType

## Summary

The builder `crate::queries::typing::build_expected_type`
(`queries/typing`) looks at the parent node of the `ExprRoot` and
decides the expected type:

- `Component { kind: Reg | OutgoingReg }` -> `Some(Type::Clock)` (reg driver
  expressions are clocked).
- `Driver(_)` -> resolve the LHS path (rewriting `it`/`it.x` references to the
  enclosing submodule/component/socket name), look up the containing `ModDef`
  symbol, fetch `ComponentAnalysis`, and return
  `component_analysis.type_of(lhs_path)` (the type of the driven component),
  or `None` if unresolvable.
- `ModDefStmtWhen` -> `Some(Type::Bit)` (guard expressions must be `Bit`).
- `ModDefStmtMatch` -> `None` (the subject's type is unconstrained; inference
  decides).
- `ExprWhen` -> guard child -> `Some(Type::Bit)`; body child -> recurse via
  `build_expected_type` on the parent `ExprWhen`'s `ExprRoot` to inherit the
  enclosing expected type.
- `ExprMatch` -> subject child -> `None`; body children -> recurse to inherit
  from the parent `ExprMatch`.
- `Enumerant(_)` -> look up the enclosing `EnumDef`, fetch its `TypeDef`, return
  `typedef.width.map(Type::Word)` (the enum's backing word width).
- Anything else -> `todo!()` panic.

## Signature

```rust
ExpectedType(exprroot: ExprRoot) -> Option<Type>
```

## Result

The type an expression root is *expected* to have, derived from its syntactic
context - or `None` if the context does not constrain it (let inference decide).

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
* [`SymbolTable`](SymbolTable.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`TypeDef`](TypeDef.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
out := !last && inp
```

The `!last && inp` root's parent is a `Driver`. Resolving the LHS path `out`
yields the `out` component, whose type is `Bit`, so `ExpectedType` returns
`Some(Type::Bit)`. The typechecker then *checks* (rather than infers) that
`!last && inp` has type `Bit`.

For the `when { case reset { ... } }` guard, the guard expression's parent is
a `ModDefStmtWhen`, so `ExpectedType` returns `Some(Type::Bit)`.

For `match opcode { ... }`, the subject `opcode`'s parent is a
`ModDefStmtMatch`, so `ExpectedType` returns `None` - inference determines that
`opcode` is `Opcode`.
