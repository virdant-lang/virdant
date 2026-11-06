`timescale 10ns/1ns

// Regression testbench for the statement-level `else match` arm.
//
// After reset deasserts with `go` held high, both FSMs must reach
// #Running (encoding 1) on the next clock edge.

module Tb;
    reg clock = 0;
    reg reset = 1;
    reg go    = 1;

    \top::Top dut(
        .clock(clock),
        .reset(reset),
        .go(go)
    );

    always #1 clock = ~clock;

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);

        repeat (3) @(posedge clock);
        reset = 0;
        repeat (2) @(posedge clock);
        #0.1;

        if (dut.state_flat !== 1'b1) begin
            $display("FAIL: state_flat should be #Running (1), got %b", dut.state_flat);
            $fatal;
        end
        if (dut.state_else_match !== 1'b1) begin
            $display("FAIL: state_else_match should be #Running (1), got %b (else match dropped?)", dut.state_else_match);
            $fatal;
        end

        $display("DONE");
        $finish;
    end
endmodule
