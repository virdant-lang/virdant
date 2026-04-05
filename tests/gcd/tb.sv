`timescale 10ns/1ns

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    wire [7:0] result;
    wire valid;

    reg  [7:0] x;
    reg  [7:0] y;
    reg  fire;

    Top dut(
        .clock(clock),
        .reset(reset),
        .result(result),
        .valid(valid),
        .x(x),
        .y(y),
        .fire(fire)
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

    task assert_gcd(input [7:0] x_val, input [7:0] y_val, input [7:0] expected);
    begin
        @(posedge clock);
        x = x_val;
        y = y_val;
        fire = 1;
        #0.1;
        @(posedge clock);
        fire = 0;
        #0.1;
        wait(valid);

        $display("GCD(%d, %d): actual = %d   expected = %d", x_val, y_val, dut.result, expected);
        if (dut.result != expected) begin
            $fatal;
        end
        @(posedge clock);
    end
    endtask

    wire [2:0] state;
    assign state = dut.gcd.state[2:0];

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);
        x = 0;
        y = 0;
        fire = 0;
        #0.1;

        do_reset;
        assert_gcd(12, 15, 3);
        assert_gcd(15, 12, 3);
        assert_gcd(7, 12, 1);
        assert_gcd(1, 1, 1);
        assert_gcd(2, 2, 2);
        assert_gcd(10, 10, 10);
        assert_gcd(6, 4, 2);
        assert_gcd(4, 6, 2);
        assert_gcd(15, 10, 5);
        assert_gcd(100, 75, 25);
        assert_gcd(48, 18, 6);
        assert_gcd(18, 48, 6);
        assert_gcd(56, 98, 14);
        assert_gcd(98, 56, 14);
        assert_gcd(17, 13, 1);
        assert_gcd(13, 17, 1);
        assert_gcd(255, 85, 85);
        assert_gcd(85, 255, 85);
        assert_gcd(120, 45, 15);
        assert_gcd(45, 120, 15);
        assert_gcd(81, 27, 27);
        assert_gcd(27, 81, 27);
        assert_gcd(64, 48, 16);
        assert_gcd(48, 64, 16);
        assert_gcd(105, 70, 35);
        assert_gcd(70, 105, 35);
        assert_gcd(30, 42, 6);
        assert_gcd(42, 30, 6);
        assert_gcd(7, 1, 1);
        assert_gcd(1, 7, 1);
        assert_gcd(1, 1, 1);
        assert_gcd(0, 0, 0);

        $display("DONE");
        $finish;
    end
endmodule
