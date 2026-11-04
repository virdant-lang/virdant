`timescale 10ns/1ns

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    wire fin;

    \add_carry::Top dut(
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

        // 10 + 3 == 13, carry clear.
        if (dut.sum_nc !== 5'd13) $fatal(1, "assert failed: x +. y == 13");
        if (dut.sum_nc[4] !== 1'b0) $fatal(1, "assert failed: no carry out");

        // 15 + 1 == 16, carry set.
        if (dut.sum_c !== 5'b10000) $fatal(1, "assert failed: max +. 1 == 16");
        if (dut.sum_c[4] !== 1'b1) $fatal(1, "assert failed: carry out set");

        // 10 - 3 == 7, borrow clear.
        if (dut.diff_nb !== 5'd7) $fatal(1, "assert failed: x -. y == 7");
        if (dut.diff_nb[4] !== 1'b0) $fatal(1, "assert failed: no borrow out");

        // 3 - 10 == 25 (mod 32), borrow set.
        if (dut.diff_b !== 5'b11001) $fatal(1, "assert failed: y -. x == 25");
        if (dut.diff_b[4] !== 1'b1) $fatal(1, "assert failed: borrow out set");

        // 15 - 0 == 15, borrow clear.
        if (dut.diff_zero !== 5'd15) $fatal(1, "assert failed: max -. 0 == 15");
        if (dut.diff_zero[4] !== 1'b0) $fatal(1, "assert failed: no borrow out (15 - 0)");

        $display("DONE");
        $finish;
    end
endmodule
