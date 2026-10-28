# sim/uart

UART design (`src/uart.vir`, 12 MHz clock). Unlike the other sim projects,
this one is lint-only: the Makefile runs `verilator --lint-only -Wno-MULTITOP`
on the generated Verilog instead of a testbench. The project's
`Virdant.toml` was previously misnamed (`basics`); it is now `uart`.
