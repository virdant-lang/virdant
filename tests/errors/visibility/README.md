# visibility

`pub` surface errors — what may reference non-published items: leaks
through published surfaces (ports, types, submodule definitions),
`for`-clause modules referencing non-pub items, and related rules.

One diagnostic family per file, each ending with the standard
`// CHECK: Check failed` trailer. The `pub_notpub/` subdirectory holds the
project-style variant of this family (see its README). See the source
`.vir` files for the exact coverage.
