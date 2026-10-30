`timescale 10ns/1ns

// Testbench for the implicit-clock rule: `count` is a reg without an
// `on` clause in a module whose only Clock-typed component is `clock`.
// The reg must be clocked by that clock and increment once per cycle.

module Tb;
    reg  clock = 0;
    wire [3:0] out;

    \top::Top dut(
        .clock(clock),
        .out(out)
    );

    always begin
        #1 clock = ~clock;
    end

    task check(input [3:0] expected);
    begin
        // Wait for the NBA to update count after a posedge, then check.
        #0.1;
        if (out !== expected) begin
            $display("FAIL: expected %0d, got %0d at time %0t",
                     expected, out, $time);
            $fatal;
        end
        $display("PASS [%0t] count = %0d", $time, out);
    end
    endtask

    initial begin
        $dumpfile("build/dump.vcd");
        $dumpvars(0, Tb);

        // count starts at 0 via `initial it <= 0`.
        check(4'd0);

        // Each posedge increments the implicitly-clocked reg.
        @(posedge clock);  check(4'd1);
        @(posedge clock);  check(4'd2);
        @(posedge clock);  check(4'd3);
        @(posedge clock);  check(4'd4);
        @(posedge clock);  check(4'd5);
        @(posedge clock);  check(4'd6);
        @(posedge clock);  check(4'd7);
        @(posedge clock);  check(4'd8);

        $display("DONE");
        $finish;
    end
endmodule
