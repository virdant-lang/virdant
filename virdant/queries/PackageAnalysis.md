# Query: PackageAnalysis

## Summary

The builder `crate::queries::build_package_analysis`
(`analysis/package`) fetches the `Parsing` and the global `Packages`
list, then constructs `PackageAnalysis::new`. Construction runs
three passes:

1. `add_imports` - walks root children; for each `Import`
   payload, inserts the target into `imports` (seeding `builtin` first), emitting
   `DuplicateImport` on collision.
2. `add_items` - walks root children; for each item, records
   `name -> AstNodeId` and (unless it contains syntax errors) recursively
   collects expression roots via `add_item_expr_roots` and its
   helpers, which traverse ModDefs, EnumDefs, drivers,
   submodules, sockets, and `when`/`match` statements. A `MissingOnClause`
   diagnostic is emitted for `Reg`/`OutgoingReg` components without a clock.
3. `validate_imports` - emits `UnresolvedImportError` for any
   imported package not in the known `Packages` list.

`PackageAnalysis` is the foundation of the analysis layer: `SymbolTable`
iterates every package's `item_names()`, and `ExprRoots` gathers
`expr_roots_node_ids()` across all packages.

## Signature

```rust
PackageAnalysis(package: PackageFqn) -> Arc<PackageAnalysis>
```

## Result

Per-package structural analysis: the imports, the top-level items, the
expression roots, and the structural diagnostics, all gathered by walking one
package's parsed AST.

### `PackageAnalysis`

```rust
#[derive(Debug)]
pub struct PackageAnalysis {
    package: PackageFqn,
    imports: IndexSet<PackageFqn>,
    items: IndexMap<BString, Vec<AstNodeId>>,
    expr_roots: Vec<AstNodeId>,
    diagnostics: Vec<Diagnostic>,
}
```

Defined in `virdant/src/analysis/package`.

- `package` - the `PackageFqn` this analysis describes.
- `imports` - the set of imported packages, in insertion order, deduplicated.
  Always seeded with the `builtin` package. Each `import` statement adds its
  target here; duplicates produce a `DuplicateImport` diagnostic.
- `items` - top-level item declarations (modules, structs, unions, enums, etc.)
  keyed by their byte-string name. The value is a `Vec<AstNodeId>` because
  duplicate names can occur (each node id is pushed); a `DuplicateItem`
  diagnostic is emitted on collision.
- `expr_roots` - AST node IDs of every expression root in the package: driver
  RHS expressions, `when`/`match` guards and subjects, enumerant values, and
  register clock expressions. These are the entry points the typechecker
  processes. Items containing syntax errors are skipped.
- `diagnostics` - structural diagnostics produced during construction:
  `UnresolvedImportError`, `DuplicateImport`, `DuplicateItem`, `MissingOnClause`.

## Dependencies

* [`Parsing`](Parsing.md)
* [`Packages`](Packages.md)

## Example

For `tests/pass/typedefs/src/top.vir`:

```virdant
enum type Opcode {
    Add = 1w4
    Sub = 2
    ...
}

struct type Color {
    red : Word[8]
    ...
}

export mod Top {
    incoming opcode_bits : Word[4]
    ...
    wire opcode : Opcode
    opcode := cast(opcode_bits)

    out := match opcode {
        case #Add => x + y
        ...
    }
}
```

`PackageAnalysis("top")` records `imports = {builtin}` (the implicit import),
`items = {"Opcode" -> [...], "Color" -> [...], "Top" -> [...]}`, and
`expr_roots` containing the `cast(opcode_bits)` node, the `match opcode` subject,
and each arm body (`x + y`, etc.). No diagnostics are produced for this valid
file.
