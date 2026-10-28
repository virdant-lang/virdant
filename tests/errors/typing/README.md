# typing

Type-checking errors (one family per file):

- `driver_type_mismatch.vir` — drivers assigning mismatched types
  (was `errors/basic_typechecking.vir`).
- `builtin_type_arity.vir` — wrong arity on builtin type references like
  `Word[]` (was `errors/types.vir`).
- `word_literals.vir` — word-literal width errors.
- `when_condition_type.vir` — `when` conditions must be `Bit`
  (was `errors/clock.vir`).
- `index_bounds.vir` — out-of-bounds indexing and slicing
  (was `errors/index_out_of_bounds.vir`).
- `infer_binop.vir`, `infer_mux.vir` — inference failures for binary
  operators and muxes from either side.
- `enum_values.vir` — DuplicateEnumValue and EnumPartiallySpecified rules
  (merged from the old `duplicate_enum_value.vir` +
  `enum_partially_specified.vir`).
- `generics/` — generic-parameterization errors; see that directory's
  README.
