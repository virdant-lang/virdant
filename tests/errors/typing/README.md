# typing

Type-checking errors, one diagnostic family per file: driver type
mismatches, builtin type arity, word literals, `when`-condition types,
index bounds, inference failures, and enum value rules.

Files are grouped by diagnostic family; `generics/` holds the
generic-parameterization errors (see that directory's README). Each file
ends with the standard `// CHECK: Check failed` trailer. See the source
`.vir` files for the exact coverage.
