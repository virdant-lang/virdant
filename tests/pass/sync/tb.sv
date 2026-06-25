`timescale 10ns/1ns

module Tb;
    reg        clock = 0;
    reg        reset = 0;
    reg        inp_async = 0;
    wire       out_sync;
    wire [7:0] cycle_count;

    Top dut(
        .clock(clock),
        .reset(reset),
        .inp_async(inp_async),
        .out_sync(out_sync),
        .cycle_count(cycle_count)
    );

    always begin
        #1 clock = ~clock;
    end

    task do_reset;
    begin
        reset = 1;
        @(posedge clock);
        reset = 0;
        @(posedge clock);
    end
    endtask

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, dut);

        do_reset;

        // After reset, out_sync should be 0
        if (out_sync !== 1'b0) $fatal(1, "After reset: out_sync should be 0");

        // Drive inp_async high, wait 2 clocks, check out_sync follows
        inp_async = 1;
        @(posedge clock);
        @(posedge clock);
        if (out_sync !== 1'b1) $fatal(1, "Test 1: out_sync should be 1 after 2 clocks");

        // Drive inp_async low, wait 2 clocks, check out_sync follows
        inp_async = 0;
        @(posedge clock);
        @(posedge clock);
        if (out_sync !== 1'b0) $fatal(1, "Test 2: out_sync should be 0 after 2 clocks");

        // Toggle high again
        inp_async = 1;
        @(posedge clock);
        @(posedge clock);
        if (out_sync !== 1'b1) $fatal(1, "Test 3: out_sync should be 1 after 2 clocks");

        // Toggle low again
        inp_async = 0;
        @(posedge clock);
        @(posedge clock);
        if (out_sync !== 1'b0) $fatal(1, "Test 4: out_sync should be 0 after 2 clocks");

        $display("DONE");
        $finish;
    end
endmodule