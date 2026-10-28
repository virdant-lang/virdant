# drivers

Driver-structure errors — what may be driven, how, and when:

- `drivers.vir` — core driver errors (illegal targets and drivers).
- `drive_sink.vir` — driving a sink (e.g. `incoming`) is an error.
- `when_drivers.vir` — driver rules inside `when` blocks.
- `missing_on_clause.vir` — a register declared without a required `on`
  clock clause.
- `initial_errors.vir` — illegal `initial` statements (was
  `errors/reg_initial_errors.vir`).
- `redundant_unused.vir` — `unused` statements for components that are
  actually driven, or redundant driver declarations.
