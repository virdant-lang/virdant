# sim/sockets

Behavioral test of client/server sockets between module instances. The top
module wires a `Core` and a `Memory` instance together with
`memory.mem :=: core.mem`, and the testbench drives requests through the
generated Verilog and asserts the memory responses.
