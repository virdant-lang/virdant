`timescale 10ns/1ns

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    wire fin;

    Top dut(
        .clock(clock),
        .reset(reset),
        .fin(fin)
    );

    task do_reset;
    begin
        reset = 0;
        #0.1;
        @(posedge clock);

        reset = 1;
        #0.1;
        @(posedge clock);

        reset = 0;
        #0.1;
        @(posedge clock);
    end
    endtask

    always begin
        #1 clock = ~clock;
    end

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, dut);

        do_reset;
        repeat (10) @(posedge clock);

        $display("DONE");
        $finish;
    end
endmodule
