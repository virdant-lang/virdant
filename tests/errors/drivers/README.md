# drivers

Driver-structure errors — what may be driven, how, and when: illegal
driver targets, driving sinks, driver rules inside `when` blocks, missing
`on` clock clauses, illegal `initial` statements, and redundant or unused
driver declarations.

One diagnostic family per file, each ending with the standard
`// CHECK: Check failed` trailer. See the source `.vir` files for the exact
coverage.
