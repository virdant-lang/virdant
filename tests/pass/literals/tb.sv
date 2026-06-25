`timescale 10ns/1ns

// Testbench for literal forms.
//
// Verifies that explicit-width, inferred-width, binary, hex, and
// underscore-separated literals produce the correct values.
// Uses hierarchical references to access internal wires.

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    wire fin;

    Top dut(
        .clock(clock),
        .reset(reset),
        .fin(fin)
    );

    always begin
        #1 clock = ~clock;
    end

    initial begin
        $dumpfile("build/dump.vcd");
        $dumpvars(0, Tb);

        // Wait for reset+NBA
        reset = 1;
        @(posedge clock);
        reset = 0;
        @(posedge clock);
        #0.1;

        // Explicit-width literal: 0w4 = 4'b0000
        if (dut.explicit !== 4'h0) $fatal(1, "explicit: expected 4'h0");

        // Inferred-width literal: 0 => Word[4] = 4'b0000
        if (dut.inferred !== 4'h0) $fatal(1, "inferred: expected 4'h0");

        // Binary literal: 0b1010 => Word[4] = 4'hA
        if (dut.bin !== 4'hA) $fatal(1, "bin: expected 4'hA");

        // Hex literal: 0xfe => Word[8] = 8'hFE
        if (dut.hex !== 8'hFE) $fatal(1, "hex: expected 8'hFE");

        // Explicit-width binary: 0b1010w4 = 4'hA
        if (dut.explicit_bin !== 4'hA) $fatal(1, "explicit_bin: expected 4'hA");

        // Explicit-width hex: 0xfew8 = 8'hFE
        if (dut.explicit_hex !== 8'hFE) $fatal(1, "explicit_hex: expected 8'hFE");

        // Underscore-separated decimal: 100_000 = 32'd100000
        if (dut.underscore !== 32'd100000) $fatal(1, "underscore: expected 100000");

        // Underscore-separated binary: 0b00_101_11 = 7'b010111
        if (dut.underscore_bin !== 7'b010111) $fatal(1, "underscore_bin: expected 7'b010111");

        // Underscore-separated hex: 0xcafe_babe = 32'hcafebabe
        if (dut.underscore_hex !== 32'hcafebabe) $fatal(1, "underscore_hex: expected 32'hcafebabe");

        $display("DONE");
        $finish;
    end
endmodule