# syntax

Parser and lexer errors: syntax errors, illegal characters such as tabs,
indentation rules, and docstring spacing.

Each `.vir` file must fail `vir check`, and its own `// CHECK:` /
`// CHECK-NOT:` directives assert the exact diagnostics. Files are grouped
here when the diagnostic comes from parsing or lexing rather than from the
analyser. See the source `.vir` files for the exact coverage.
