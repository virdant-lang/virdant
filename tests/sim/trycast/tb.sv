`timescale 10ns/1ns

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    wire fin;

    \top::Top dut(
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
        do_reset;
        repeat (3) @(posedge clock);

        // Valid encodings trycast to @Valid(phase): the one-hot
        // `Valid` tag (2'b10) above the payload.
        if (dut.off_try !== 5'b00010) $fatal(1, "trycast(0w3) == @Valid(#Off)");
        if (dut.green_try !== 5'b00110) $fatal(1, "trycast(1w3) == @Valid(#Green)");
        if (dut.yellow_try !== 5'b01010) $fatal(1, "trycast(2w3) == @Valid(#Yellow)");
        if (dut.red_try !== 5'b10010) $fatal(1, "trycast(4w3) == @Valid(#Red)");

        // Non-encodings trycast to @Invalid(): tag 2'b01, zeros above.
        if (dut.invalid_three !== 5'b00001) $fatal(1, "trycast(3w3) == @Invalid()");
        if (dut.invalid_five !== 5'b00001) $fatal(1, "trycast(5w3) == @Invalid()");
        if (dut.invalid_six !== 5'b00001) $fatal(1, "trycast(6w3) == @Invalid()");
        if (dut.invalid_seven !== 5'b00001) $fatal(1, "trycast(7w3) == @Invalid()");

        $display("trycast sim OK");
        $finish;
    end
endmodule
