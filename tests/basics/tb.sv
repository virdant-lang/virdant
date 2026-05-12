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
    always @(posedge clock) begin
        if (!reset) begin
            if (!(dut.w)) $fatal(1, "Assertion failed: w");
            if (dut.a) $fatal(1, "Assertion failed: !a");
            if (dut.w != 1'b1) $fatal(1, "Assertion failed: w == true");
            if (dut.a != 1'b0) $fatal(1, "Assertion failed: a == false");
            if (dut.w == dut.a) $fatal(1, "Assertion failed: w != a");

            if (!(dut.w && (!dut.a))) $fatal(1, "Assertion failed: w && (!a)");
            if (!(dut.w || dut.a)) $fatal(1, "Assertion failed: w || a");
            if (!(dut.w ^ dut.a)) $fatal(1, "Assertion failed: w ^^ a");
            if (!(dut.w ? 1'b1 : 1'b0)) $fatal(1, "Assertion failed: if w...");
            if (!(dut.a ? 1'b0 : 1'b1)) $fatal(1, "Assertion failed: if a...");

            if (dut.x != 4'b1010) $fatal(1, "Assertion failed: x == 0b1010w4");
            if (dut.y != 4'b0011) $fatal(1, "Assertion failed: y == 0b0011w4");
            if (dut.x == dut.y) $fatal(1, "Assertion failed: x != y");
            if (!(dut.x > dut.y)) $fatal(1, "Assertion failed: x > y");
            if (!(dut.y < dut.x)) $fatal(1, "Assertion failed: y < x");
            if (!(dut.x >= 4'b1010)) $fatal(1, "Assertion failed: x >= 0b1010w4");
            if (!(dut.y <= dut.x)) $fatal(1, "Assertion failed: y <= x");

            if ((dut.x + dut.y) != 4'b1101) $fatal(1, "Assertion failed: x + y == 0b1101w4");
            if ((dut.x - dut.y) != 4'b0111) $fatal(1, "Assertion failed: x - y == 0b0111w4");
            if (~dut.x != 4'b0101) $fatal(1, "Assertion failed: ~x == 0b0101w4");
            if (4'b1111 != 4'b1111) $fatal(1, "Assertion failed: -1w4 == 0b1111w4");

            if (dut.w != 1'b1) $fatal(1, "Assertion failed: word(w: Bit) == 1w1");
            if ({dut.w, dut.y} != 5'b10011) $fatal(1, "Assertion failed: word(w, y) == 0b10011w5");
            if (!dut.w) $fatal(1, "Assertion failed: word(w, y)[4]");
            if (dut.y[3] != 1'b0) $fatal(1, "Assertion failed: word(w, y)[3] == false");
            if (dut.y[2] != 1'b0) $fatal(1, "Assertion failed: word(w, y)[2] == false");
            if (!dut.y[1]) $fatal(1, "Assertion failed: word(w, y)[1]");
            if (!dut.y[0]) $fatal(1, "Assertion failed: word(w, y)[0]");

            if (dut.z != 8'h08) $fatal(1, "Assertion failed: z == 0x08w8");
            if (dut.z[7] != 1'b0) $fatal(1, "Assertion failed: z[7] == false");
            if (dut.s != 8'hf8) $fatal(1, "Assertion failed: s == 0xf8w8");
            if (dut.s[7] != 1'b1) $fatal(1, "Assertion failed: s[7] == true");

            if ((dut.w ? dut.x : dut.y) != dut.x) $fatal(1, "Assertion failed: if w { x } else { y } == x");
            if ((dut.a ? dut.x : dut.y) != dut.y) $fatal(1, "Assertion failed: if a { x } else { y } == y");

            if (dut.bit_and) $fatal(1, "Assertion failed: !bit_and");
            if (!dut.bit_or) $fatal(1, "Assertion failed: bit_or");
            if (!dut.bit_xor) $fatal(1, "Assertion failed: bit_xor");

            if (dut.\or  != 8'd1) $fatal(1, "Assertion failed: or == 1");
            if (dut.\and  != 8'd1) $fatal(1, "Assertion failed: and == 1");
            if (dut.\xor  != 8'd1) $fatal(1, "Assertion failed: xor == 1");

            if (dut.ascription != 4'd15) $fatal(1, "Assertion failed: ascription == 15");
        end
    end
endmodule
