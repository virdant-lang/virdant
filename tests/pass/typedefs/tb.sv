`timescale 10ns/1ns

module Tb;
    reg  [7:0] x;
    reg  [7:0] y;
    reg  [3:0] opcode_bits;
    wire [7:0] out;
    wire [7:0] red;

    Top dut(
        .x(x),
        .y(y),
        .opcode_bits(opcode_bits),
        .out(out),
        .red(red)
    );

    task assert_(input [7:0] x_val, input [7:0] y_val, input [3:0] op, input [7:0] expected);
    begin
        x = x_val;
        y = y_val;
        opcode_bits = op;
        #0.1;

        if (dut.out != expected) begin
            $display("Error assert_(%d, %d, %d, %d)", x_val, y_val, op, expected);
            $fatal;
        end
    end
    endtask

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);

        assert_(7, 5, 4'b0001, 12);
        assert_(7, 5, 4'b0010, 2);
        assert_(7, 5, 4'b0100, 5);
        assert_(7, 5, 4'b1000, 7);

        if (dut.red != 128) begin
            $display("Error dut.red != 128, was %d instead.\n", dut.red);
            $fatal;
        end

        $display("DONE");
        $finish;
    end
endmodule
