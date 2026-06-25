`timescale 10ns/1ns

// Testbench for rising-edge detector.
//
// The DUT samples inp on posedge clock into reg "last", then computes
// out = !last && inp as combinational logic.  This produces a high
// output when inp transitions from 0 to 1 between clock edges.
//
// After a posedge where inp is captured, last equals the sampled value,
// so out goes low again on the next clock edge (when last catches up).

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    reg  inp   = 0;
    wire out;

    Top dut(
        .clock(clock),
        .reset(reset),
        .inp(inp),
        .out(out)
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
        $dumpfile("build/dump.vcd");
        $dumpvars(0, Tb);

        do_reset;

        // After reset: last = 0 (captured at last posedge), inp = 0
        // out = !0 && 0 = 0
        if (out !== 1'b0) $fatal(1, "After reset: out should be 0");

        // Wait for a posedge.  At this edge, last captures inp (= 0).
        @(posedge clock);
        #0.1;

        // Still: last = 0, inp = 0, out = 0
        if (out !== 1'b0) $fatal(1, "After clock: out should be 0");

        // Drive inp high between clock edges.  out goes high immediately
        // because last (= 0) differs from inp (= 1).
        inp = 1;
        #0.1;
        if (out !== 1'b1) $fatal(1, "Rising edge: out should be 1");

        // At next posedge, last captures inp (= 1).  After that, out goes low
        // because last == inp.
        @(posedge clock);
        #0.1;
        if (out !== 1'b0) $fatal(1, "After capture: out should be 0");

        // Keep inp high; out stays 0 since last == inp.
        @(posedge clock);
        #0.1;
        if (out !== 1'b0) $fatal(1, "Stayed high: out should be 0");

        // Drive inp low between edges.  out stays low (falling edge).
        inp = 0;
        #0.1;
        if (out !== 1'b0) $fatal(1, "Falling edge: out should be 0");

        // At next posedge, last captures inp (= 0).
        @(posedge clock);
        #0.1;
        if (out !== 1'b0) $fatal(1, "After falling edge clock: out should be 0");

        // Keep inp low; out stays 0.
        @(posedge clock);
        #0.1;
        if (out !== 1'b0) $fatal(1, "Still low: out should be 0");

        // Drive inp high again between edges.  out goes high.
        inp = 1;
        #0.1;
        if (out !== 1'b1) $fatal(1, "Second rising edge: out should be 1");

        // Next posedge captures inp (= 1); out goes low.
        @(posedge clock);
        #0.1;
        if (out !== 1'b0) $fatal(1, "After second capture: out should be 0");

        // Drive inp low, then wait a posedge, then drive high again.
        inp = 0;
        @(posedge clock);
        #0.1;
        inp = 1;
        #0.1;
        if (out !== 1'b1) $fatal(1, "Third rising edge: out should be 1");

        $display("DONE");
        $finish;
    end
endmodule