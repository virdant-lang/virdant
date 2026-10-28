# cycles

Cycle-detection errors: combinational loops through driver dependencies
(including the `dependson` edge flavor) and module-definition cycles
(simple, chained, and self-referential).

One diagnostic family per file, each ending with the standard
`// CHECK: Check failed` trailer. See the source `.vir` files for the exact
coverage.
