`timescale 10ns/1ns

// Drives the monomorphized generic design:
//   - the Master[Word[8]]/Slave[Word[8]] socket pair passes `a`
//     combinationally through the `:=:` connection, so out8 follows a;
//   - Reg[Word[4]]/Tagged[Word[4]] latch on the clock, so out4
//     reflects b one cycle later, at Word[4] width.
module Tb;
    reg  clk = 0;
    reg  [7:0] a = 8'hA5;
    reg  [3:0] b = 4'h3;
    wire [7:0] out8;
    wire [3:0] out4;

    \top::Top dut(
        .clk(clk),
        .a(a),
        .b(b),
        .out8(out8),
        .out4(out4)
    );

    always #1 clk = ~clk;

    initial begin
        #0.1;
        if (out8 !== 8'hA5) $fatal(1, "out8 should pass a through Bus[Word[8]]");

        @(posedge clk);
        #0.1;
        if (out4 !== 4'h3) $fatal(1, "out4 should latch b=3 through Tagged[Word[4]]");

        a = 8'h5A;
        b = 4'hC;
        #0.1;
        if (out8 !== 8'h5A) $fatal(1, "out8 should follow the new a");

        @(posedge clk);
        #0.1;
        if (out4 !== 4'hC) $fatal(1, "out4 should latch the new b=12");

        $display("DONE");
        $finish;
    end
endmodule
