# Query: DependencyGraph

## Summary

The builder `crate::analysis::dependency::build_dependency_graph`
(`dependency`) seeds `nodes` with every component from
`ComponentAnalysis`, then calls two walkers:

- `collect_driver_edges` (`dependency`) walks each `Driver` statement,
  resolves the target to a `Component`, picks `Combinational` for
  `DriverType::Continuous` and `Sequential` for `DriverType::Latched`, then
  walks the RHS expression adding an edge `target -> dependee` for every
  `ExprReference` resolving to a known component. `BidirectionalDriver` pairs
  add a combinational edge from the sink side to the source side. `Submodule`/
  `Socket`/`Component` with an `It` block recurse with an extended `it_context`.
- `collect_dependson_edges` (`dependency`) walks `ModDefStmtDependsOn`
  (an explicit `dependson lhs = rhs` declaration), resolving both sides to
  components and adding a `Combinational` edge `dependent -> dependee`.

Helpers: `add_edge` skips self-loops; `resolve_it_path`/`resolve_path_component`
expand `it`-prefixed paths against the enclosing `it_context`.

`DependencyGraph` is consumed by `CombinationalCycleCheck`, which stitches the
per-module graphs along the instance tree and looks for combinational cycles.

## Signature

```rust
DependencyGraph(symbol_id: SymbolId) -> Arc<DependencyGraph>
```

## Result

A dependency graph over the signals of a single module: which component depends
on which, with each edge labelled combinational or sequential.

### `DependencyGraph`

```rust
/// A dependency graph over the signals of a single module.
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    /// All components in the module.
    nodes: IndexSet<ComponentId>,

    /// For each component, the components it depends on.
    edges: IndexMap<ComponentId, Vec<DependencyEdge>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependencyEdge {
    pub dependee: ComponentId,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EdgeKind {
    /// Combinational: source changes in the same cycle when dependee
    /// changes.
    Combinational,

    /// Sequential: source reflects dependee's value at the next clock
    /// tick.
    Sequential,
}
```

Defined in `virdant/src/analysis/dependency`.

- `nodes` - every component declared in the module, insertion-ordered and
  deduplicated.
- `edges` - adjacency map from a *dependent* component to its `DependencyEdge`s.
  An edge `source -> dependee` means "source depends on dependee" (source's
  value is computed from dependee's).
- `DependencyEdge.dependee` - the component being depended upon.
- `DependencyEdge.kind` - `Combinational` (same-cycle, from a continuous driver
  `=`) or `Sequential` (next-cycle, from a latched driver `<=`).

Self-loops are skipped by the `add_edge` helper (`dependency`).
Accessors: `nodes()`, `edges()`, `dependencies_of(component)`.

## Dependencies

* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
reg last : Bit on clock {
    it <= inp
}

out := !last && inp
```

`DependencyGraph(<Top id>)` records `nodes = {clock, reset, inp, out, last}`.
The `out := !last && inp` driver contributes edges `out -> last` and
`out -> inp` (both combinational, since `:=` is continuous). The
`it <= inp` reg driver contributes `last -> inp` (sequential, since `<=` is
latched). `clock` and `reset` have no dependents unless referenced in a driver.
