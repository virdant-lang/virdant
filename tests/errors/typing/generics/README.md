# typing/generics

Errors in generic type and socket parameterization, one diagnostic family
per file: type-argument count, missing or unexpected type arguments,
`ext mod` generic rules, type-argument kind mismatches, socket connections
instantiated at different type arguments, and unbound generic parameters.

Each file ends with the standard `// CHECK: Check failed` trailer. See the
source `.vir` files for the exact coverage.
