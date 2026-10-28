# sim/basics

Core-operators behavioral test: the top module (`src/basic.vir`) drives the
same values through different expressions, and `tb.sv` asserts every one of
them through the generated Verilog — bitwise logic, comparisons,
concatenation, muxes, ascription, truncation, and word/bit literals.

Note: the top module lives in `basic.vir`, so the generated package is
`basic` and the testbench instantiates `\basic::Top` (not `top::Top`).
