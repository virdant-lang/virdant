# typing/generics

Errors in generic type and socket parameterization, one family per file:

- `arity.vir` — TypeArgCount errors: a type reference supplying the wrong
  number of type arguments, at both the definition-instantiation sites
  (merged from the old `generics_arity.vir` +
  `generics_instantiate_arity.vir`).
- `ctor_requires_args.vir` — a generic type used without type arguments.
- `param_takes_no_args.vir` — a non-generic type given type arguments.
- `ext_mod.vir` — `ext mod` cannot have generics, at both the declaration
  site and the instantiation site (merged from `generics_ext_mod.vir` +
  `generics_ext_mod_args.vir`).
- `kind_mismatch.vir` — TypeArgKind errors: a Nat parameter given a type,
  a Type parameter given a Nat literal, and unknown kinds (merged from
  `generics_kind_nat.vir` + `generics_kind_type.vir` +
  `generics_unknown_kind.vir`).
- `socket_connect_mismatch.vir` — connecting sockets instantiated at
  different type arguments.
- `unbound_param.vir` — an unresolved generic parameter.
