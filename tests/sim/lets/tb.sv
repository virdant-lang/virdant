`timescale 10ns/1ns

module Tb;
    reg  clock = 0;
    reg  reset = 0;
    wire done;

    \lets::Top dut(
        .clock(clock),
        .reset(reset),
        .done(done)
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

        // Untyped binding, inferred from its use.
        if (dut.a !== 1'b1) $fatal(1, "Assertion failed: a (let x = 0w0; x == 0)");

        // Multi-binding list: later bindings see earlier ones.
        if (dut.b !== 1'b1) $fatal(1, "Assertion failed: b (let x = 1w2, y = 2w2; x + y == 3)");

        // Typed binding mixed with an untyped one.
        if (dut.c !== 1'b1) $fatal(1, "Assertion failed: c (let x = 1 : Word[2], y = 2w2; x + y == 3)");

        // `let` as a match-arm body: binding scoped to the arm.
        if (dut.d !== 1'b1) $fatal(1, "Assertion failed: d (let x = 10w8 in a match arm; x == 10)");

        // A let binding shadowing the pattern-bound variable.
        if (dut.e !== 1'b1) $fatal(1, "Assertion failed: e (let value = 3w8 shadows the pattern's value; value == 3)");

        // Nested lets: an inner let inside an outer let's body.
        if (dut.f !== 1'b1) $fatal(1, "Assertion failed: f (let p = 1w2 { let q = 2w2; p + q == 3 })");

        if (dut.done !== 1'b1) $fatal(1, "Assertion failed: done (a && b && c && d && e && f)");

        $display("DONE");
        $finish;
    end

    always @(posedge clock) begin
        if (!reset) begin
            if (dut.a !== 1'b1) $fatal(1, "Assertion failed: a (let x = 0w0; x == 0)");
            if (dut.b !== 1'b1) $fatal(1, "Assertion failed: b (let x = 1w2, y = 2w2; x + y == 3)");
            if (dut.c !== 1'b1) $fatal(1, "Assertion failed: c (let x = 1 : Word[2], y = 2w2; x + y == 3)");
            if (dut.d !== 1'b1) $fatal(1, "Assertion failed: d (let x = 10w8 in a match arm; x == 10)");
            if (dut.e !== 1'b1) $fatal(1, "Assertion failed: e (let value = 3w8 shadows the pattern's value; value == 3)");
            if (dut.f !== 1'b1) $fatal(1, "Assertion failed: f (let p = 1w2 { let q = 2w2; p + q == 3 })");
            if (dut.done !== 1'b1) $fatal(1, "Assertion failed: done (a && b && c && d && e && f)");
        end
    end
endmodule
