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
    reg walk_req  = 0;

    wire green;
    wire yellow;
    wire red;
    wire walk_signal;

    Top dut(
        .clock(clock),
        .reset(reset),
        .walk_req(walk_req),
        .green(green),
        .yellow(yellow),
        .red(red),
        .walk_signal(walk_signal)
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

    // ------------------------------------------------------------------ tick
    // Advance n clock cycles and settle 0.1 ns after the last posedge so that
    // all DUT outputs are stable before the caller reads them.
    task tick;
        input integer n;
        integer i;
    begin
        for (i = 0; i < n; i = i + 1) begin
            @(posedge clock);
            #0.1;
        end
    end
    endtask

    // ---------------------------------------------------------------- helpers
    function [63:0] phase_name;
        input g, y, r, w;
    begin
        // Returned value is used only for $display; encode as a simple integer.
        phase_name = (g ? 1 : 0) | (y ? 2 : 0) | (r ? 4 : 0) | (w ? 8 : 0);
    end
    endfunction

    task show_state;
        input [80*8-1:0] label;
    begin
        if      (green)       $display("%s  -> GREEN  (G=%b Y=%b R=%b W=%b)", label, green, yellow, red, walk_signal);
        else if (yellow)      $display("%s  -> YELLOW (G=%b Y=%b R=%b W=%b)", label, green, yellow, red, walk_signal);
        else if (red)         $display("%s  -> RED    (G=%b Y=%b R=%b W=%b)", label, green, yellow, red, walk_signal);
        else if (walk_signal) $display("%s  -> WALK   (G=%b Y=%b R=%b W=%b)", label, green, yellow, red, walk_signal);
        else                  $display("%s  -> ???    (G=%b Y=%b R=%b W=%b)", label, green, yellow, red, walk_signal);
    end
    endtask

    // ------------------------------------------------------------------ main
    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);

        $display("=== Traffic Light State Machine ===");
        $display("    (G=green  Y=yellow  R=red  W=walk_signal)");
        $display("");

        do_reset;
        show_state("[ 0] after reset              ");

        // Red timer was 8 on entry; do_reset used one normal cycle (8->7),
        // so 8 more ticks bring us through the remaining decrements and the
        // transition tick into Green.
        tick(8);
        show_state("[ 1] Red  expires -> Green    ");

        tick(11);
        show_state("[ 2] Green expires -> Yellow  ");

        tick(4);
        show_state("[ 3] Yellow expires -> Red    ");

        // Press the walk button before Red's timer runs out.
        $display("      [walk button pressed]");
        walk_req = 1;
        tick(9);
        show_state("[ 4] Red  expires -> Walk     ");

        walk_req = 0;
        tick(6);
        show_state("[ 5] Walk expires -> Red      ");

        // No walk request this time — Red should go straight to Green.
        tick(9);
        show_state("[ 6] Red  expires -> Green    ");

        $display("");
        $display("DONE");
        $finish;
    end
endmodule
