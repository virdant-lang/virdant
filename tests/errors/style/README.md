# style

Style-lint diagnostics that are errors, but not type errors: for example,
constructs the grammar accepts in multiple notations but which must use one
style consistently.

One diagnostic family per file, each ending with the standard
`// CHECK: Check failed` trailer. See the source `.vir` files for the exact
coverage.
