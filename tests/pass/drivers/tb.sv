`timescale 10ns/1ns

// Testbench for the traffic-light state machine.
//
// The DUT cycles through Green -> Yellow -> Red -> (Walk if requested) -> Red.
// Cycle counts used below are derived from the timer values in top.vir:
//
//   Phase    | timer on entry | ticks to expire
//   ---------+----------------+----------------
//   Red      |  8             |  9  (8 decrements then 1 transition tick)
//   Green    | 10             | 11
//   Yellow   |  3             |  4
//   Walk     |  5             |  6
//
// Note: do_reset fires three posedges; the last one is a normal cycle that
// decrements the Red timer from 8 to 7, so only 8 more ticks are needed
// to reach the transition (not 9).

module Tb;
    reg clock     = 0;
    reg reset     = 0;

    wire out;

    Top dut(
        .clock(clock),
        .reset(reset),
        .out(out)
    );

    always #1 clock = ~clock;

    // ------------------------------------------------------------------ reset
    task do_reset;
    begin
        reset = 0;  #0.1;  @(posedge clock);
        reset = 1;  #0.1;  @(posedge clock);   // phase <- Red, timer <- 8
        reset = 0;  #0.1;  @(posedge clock);   // first normal cycle: timer 8->7
        #0.1;                                   // let combinational outputs settle
    end
    endtask

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);

        do_reset;
        repeat(10) begin
            @(posedge clock);
            if (dut.r !== 1'b1) begin
                $fatal;
            end
        end

        $display("DONE");
        $finish;
    end
endmodule
