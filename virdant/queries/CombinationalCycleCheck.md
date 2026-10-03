# Query: CombinationalCycleCheck

## Summary

The builder `crate::queries::combinational_cycles::build_combinational_cycle_check`
(`queries/combinational_cycles`) works bottom-up over the module
instantiation tree:

1. Collects every `SymbolId` whose `kind() == SymbolKind::ModDef`.
2. Computes an `inclusion_order` (leaves before parents, via DFS post-order). Back-edges (instantiation cycles) are skipped so they don't
   cause infinite recursion - those are reported separately by `check_mod_cycles`
   inside `Check`.
3. For each `moddef` in that order:
   - `build_stitched` fetches the moddef's `DependencyGraph` and
     `ComponentAnalysis`, takes only `EdgeKind::Combinational` edges, converts
     each `ComponentId -> ComponentId` edge into a `(path -> path)` entry, and
     records local edges. It then walks the moddef's `Submodule` statements,
     resolves each instantiated moddef, fetches its already-built stitched graph,
     and imports its edges with both endpoints prefixed by `inst_name.` (so a
     submodule's internal `a -> b` becomes `inst.a -> inst.b` in the parent).
   - `check_module` builds a reachability `Graph` over all
     edges and, for each *local* edge `(u, v)`, checks if `v` can reach `u` in
     the combined graph. If so, that local edge closes a cycle, and a
     `CombinationalLoop { region, components }` is emitted, where `components`
     is the cycle path `[u, v, ...back to u...]`. One report per module (the
     first cycle found).

A cycle is attributed to the module whose *local* edge closes it; cycles living
entirely inside a submodule are reported there instead.

## Signature

```rust
CombinationalCycleCheck() -> Arc<Vec<Diagnostic>>
```

## Result

Diagnostics for combinational loops across the whole program. This query takes
no arguments; it checks every `ModDef` in the symbol table.

The return type is `Arc<Vec<Diagnostic>>` (see `SyntaxErrors` for the
`Diagnostic` definition). Each loop is reported as a
`diagnostics::CombinationalLoop { region, components: Vec<BString> }`.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`DependencyGraph`](DependencyGraph.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`Parsing`](Parsing.md)
* [`SymbolAst`](SymbolAst.md)

## Example

Consider a module that feeds its own output back to its input combinationally:

```virdant
mod Loop {
    incoming a : Bit
    outgoing b : Bit

    b := a
    a := b   // combinational loop: a -> b -> a
}
```

`DependencyGraph(<Loop id>)` records `b -> a` and `a -> b` (both combinational).
`CombinationalCycleCheck()` stitches the graph and finds that the local edge
`a -> b` closes a cycle (`b` reaches `a` via `b -> a`), so it emits a
`CombinationalLoop` whose `components` is `["a", "b", "a"]` and whose `region`
is the `Loop` moddef's source span. The valid `tests/pass/edge/src/top.vir`
produces no such cycle because `last` depends on `inp` only sequentially.
