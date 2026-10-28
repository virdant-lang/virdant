# cycles

Cycle-detection errors:

- `combinational_loop.vir`, `combinational_loop_dependson.vir` —
  combinational loops through driver dependencies (including the
  `dependson` edge flavor).
- `module_cycle_simple.vir`, `module_cycle_chain.vir`,
  `module_cycle_self.vir` — module-definition cycles: simple, chained, and
  self-referential (renamed from `mod_cycle_*.vir`; the CHECK lines
  reference the new package names).
