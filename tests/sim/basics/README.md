# sim/basics

Core-operators behavioral test: the top module drives the same values
through different expressions, and `tb.sv` asserts every one of them
through the generated Verilog — bitwise logic, comparisons,
concatenation, muxes, ascription, truncation, and literals.
