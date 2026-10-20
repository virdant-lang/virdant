`timescale 10ns/1ns

// Testbench for the "lost when-arm guard" regression.
//
// The `phase`/`count` machine starts in #Wait with count = 3.  `count`
// must count down to 0 before `phase` transitions to #Go.  With the bug,
// `phase` jumps to #Go on the very first posedge.
//
// The `busy`/`done` pair is driven by a top-level `when`: while `go` is
// high, `done` must hold 0; it may only be set once `go` is low.  With
// the bug, `done` is set on the first posedge regardless of `go`.

module Tb;
    reg clock = 0;
    reg go    = 1;

    Top dut(
        .clock(clock),
        .go(go)
    );

    always #1 clock = ~clock;

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

    task check;
        input [80*8-1:0] label;
        input [1:0] exp_phase;
        input [3:0] exp_count;
        input exp_busy;
        input exp_done;
    begin
        if (dut.phase !== exp_phase || dut.count !== exp_count
            || dut.busy !== exp_busy || dut.done !== exp_done) begin
            $display("FAIL %s: phase=%b count=%d busy=%b done=%b (expected %b %d %b %b)",
                     label, dut.phase, dut.count, dut.busy, dut.done,
                     exp_phase, exp_count, exp_busy, exp_done);
            $fatal;
        end
    end
    endtask

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);

        // Phase machine: count 3 -> 2 -> 1 -> 0, staying in #Wait.
        tick(1); check("[1] count 3->2, still Wait ", 2'b01, 4'd2, 1'b1, 1'b0);
        tick(1); check("[2] count 2->1, still Wait ", 2'b01, 4'd1, 1'b1, 1'b0);
        tick(1); check("[3] count 1->0, still Wait ", 2'b01, 4'd0, 1'b1, 1'b0);

        // count == 0: the guarded transition finally fires.
        tick(1); check("[4] guarded Wait -> Go   ", 2'b10, 4'd5, 1'b1, 1'b0);

        // #Go is quiescent: count loads 0 and stays there.
        tick(1); check("[5] Go quiescent          ", 2'b10, 4'd0, 1'b1, 1'b0);

        // Drop go: busy clears and the guarded done fires.
        go = 0;
        tick(1); check("[6] go low: busy=0 done=1 ", 2'b10, 4'd0, 1'b0, 1'b1);

        $display("DONE");
        $finish;
    end
endmodule
