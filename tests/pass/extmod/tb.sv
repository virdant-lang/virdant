`timescale 10ns/1ns

// Testbench for ROM external module.
//
// Top contains an address counter (0..255) and an external `Rom` instance.
// The ROM is a synchronous 256-byte block initialized from ext/rom.hex
// with data[i] = i for all addresses.
//
// The counter increments every cycle after reset.  The ROM samples the
// address on each posedge (reads mem[addr]), so data appears one cycle
// after the corresponding address is driven.
//
// After reset:      counter=0,  addr=0
// After 1st cycle:  counter=1,  addr=1,  data=mem[0]  (= 0x00)
// After 2nd cycle:  counter=2,  addr=2,  data=mem[1]  (= 0x01)
// ...

module Tb;
    reg        clock = 0;
    reg        reset = 0;
    wire [7:0] addr;
    wire [7:0] data;
    wire       done;

    Top dut(
        .clock(clock),
        .reset(reset),
        .addr(addr),
        .data(data),
        .done(done)
    );

    always begin
        #1 clock = ~clock;
    end

    integer i;
    reg [7:0] expected_data;

    initial begin
        $dumpfile("build/dump.vcd");
        $dumpvars(0, Tb);

        // Assert reset: counter <= 0
        reset = 1;
        @(posedge clock);
        #0.1;
        reset = 0;

        // Walk through all 256 addresses.  After each posedge:
        //   counter <= counter + 1
        //   data    <= mem[counter_before_increment]
        //
        // So when addr = N, data should be mem[N-1] = N-1.
        for (i = 1; i <= 255; i = i + 1) begin
            @(posedge clock);
            #0.1;
            expected_data = i - 1;
            if (addr !== i) begin
                $display("FAIL [%0t] addr: expected 0x%02x, got 0x%02x",
                         $time, i, addr);
                $fatal;
            end
            if (data !== expected_data) begin
                $display("FAIL [%0t] data[0x%02x]: expected 0x%02x, got 0x%02x",
                         $time, i, expected_data, data);
                $fatal;
            end
            if (i < 10 || (i & 8'h1F) == 0 || i == 255) begin
                $display("PASS [%0t] addr=0x%02x data=0x%02x",
                         $time, addr, data);
            end
            // done is asserted when counter == 255 (i.e., addr == 0xFF)
            if (i == 255 && !done) begin
                $fatal(1, "done should be asserted when addr=0xFF");
            end
        end

        // One more posedge: counter wraps to 0, data = mem[0xFF] = 0xFF
        @(posedge clock);
        #0.1;
        if (addr !== 8'h00) $fatal(1, "Wrap: addr should be 0");
        if (data !== 8'hFF) $fatal(1, "Wrap: data should be 0xFF");
        $display("PASS [%0t] addr=0x%02x data=0x%02x (wrap)", $time, addr, data);

        $display("DONE");
        $finish;
    end
endmodule