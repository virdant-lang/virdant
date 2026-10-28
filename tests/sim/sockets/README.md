# sim/sockets

Behavioral test of client/server sockets between module instances: the top
module wires a `Core` and a `Memory` instance together with `:=:`, and the
testbench drives requests through the generated Verilog and asserts the
memory responses.
