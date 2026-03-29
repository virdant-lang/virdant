# UART Alice Demo

Running `make` will flash a copy of Alice in Wonderland to the Icesugar FPGA dev board's SPI flash,
compile the Virdant files in `src/` to Verilog,
run it through Yosys and NextPNR to get a bitstream,
upload the bitstream onto the device,
then open up the picocom terminal to allow you to read.

NOTE: C-a C-x to exit picocom.
