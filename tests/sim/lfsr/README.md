# sim/lfsr

Behavioral test of an LFSR (linear-feedback shift register). `src/top.vir`
defines the taps wire and the shift register; the testbench steps it
through the generated Verilog and asserts the sequence of output words.
