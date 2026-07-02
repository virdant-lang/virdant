`timescale 10ns/1ns

// Regression test for submodule socket connections placed inside an
// instantiation block (`mod X of Y { a :=: it.b }`).
//
// Master drives bus.req = 7; Slave drives bus.rsp = bus.req + 1 = 8.
// Top connects them with `m.bus :=: it.bus` inside the Slave block.
// If the converter drops that `:=:` connection, s.bus.req is undriven
// and `out` will not equal 8.
module Tb;
    wire [7:0] out;
    Top dut(.out(out));

    initial begin
        #1;
        if (out !== 8'd8) begin
            $display("FAIL: out=%0d (expected 8)", out);
            $fatal(1);
        end
        $display("PASS: out=%0d", out);
        $finish;
    end
endmodule
