`timescale 10ns/1ns

// Testbench for 8-bit Fibonacci LFSR.
//
// LFSR polynomial: feedback at bit 7, taps = 29 (0x1D = 0b00011101).
// After reset, r = 0xFF.  On each cycle:
//   r_next = {r[6:0], 1'b0} ^ (r[7] ? taps : 8'b0)
//
// Sequence starting from 0xFF:
//   0xFF -> 0xE3 -> 0xDB -> 0xAB -> 0x4B -> 0x96 -> 0x31 -> 0x62
//   -> 0xC4 -> 0x95 -> 0x37 -> 0x6E -> 0xDC -> 0xA5 -> 0x57 -> 0xAE
//   -> 0x41 -> 0x82 -> 0x19 -> 0x32 -> 0x64 -> ...

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    wire [7:0] out;

    Top dut(
        .clock(clock),
        .reset(reset),
        .out(out)
    );

    always begin
        #1 clock = ~clock;
    end

    task check(input [7:0] expected);
    begin
        // Wait for the NBA to update r after a posedge, then check.
        #0.1;
        if (out !== expected) begin
            $display("FAIL: expected 0x%02x, got 0x%02x at time %0t",
                     expected, out, $time);
            $fatal;
        end
        $display("PASS [%0t] LFSR = 0x%02x", $time, out);
    end
    endtask

    initial begin
        $dumpfile("build/dump.vcd");
        $dumpvars(0, Tb);

        // Assert reset: r loads 0xFF
        reset = 1;
        @(posedge clock);    // r <= 255 (reset asserted)
        check(8'hFF);        // verify r = 0xFF after NBA

        // De-assert reset, next cycle computes LFSR
        #0.1;
        reset = 0;
        @(posedge clock);    // r <= next(0xFF) = 0xE3
        check(8'hE3);

        // Continue sequence
        @(posedge clock);    check(8'hDB);
        @(posedge clock);    check(8'hAB);
        @(posedge clock);    check(8'h4B);
        @(posedge clock);    check(8'h96);
        @(posedge clock);    check(8'h31);
        @(posedge clock);    check(8'h62);
        @(posedge clock);    check(8'hC4);
        @(posedge clock);    check(8'h95);
        @(posedge clock);    check(8'h37);
        @(posedge clock);    check(8'h6E);
        @(posedge clock);    check(8'hDC);
        @(posedge clock);    check(8'hA5);
        @(posedge clock);    check(8'h57);
        @(posedge clock);    check(8'hAE);
        @(posedge clock);    check(8'h41);
        @(posedge clock);    check(8'h82);
        @(posedge clock);    check(8'h19);
        @(posedge clock);    check(8'h32);
        @(posedge clock);    check(8'h64);

        $display("DONE");
        $finish;
    end
endmodule