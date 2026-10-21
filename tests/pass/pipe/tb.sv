`timescale 10ns/1ns

// Drives the builtin generic Pipe[T] socket end-to-end:
//   * the client's `@Valid(d)` payload appears combinationally on the
//     server side (unwrapped by Top) at each argument's width,
//   * the server's `ready` flows back to the client,
//   * the payloads follow input changes (combinational through `:=:`).
module Tb;
    reg  [7:0] a8 = 8'hA5;
    reg  [3:0] a4 = 4'h3;
    wire       ready8;
    wire       valid8;
    wire [7:0] data8;
    wire       ready4;
    wire       valid4;
    wire [3:0] data4;

    Top dut(
        .a8     (a8),
        .a4     (a4),
        .ready8 (ready8),
        .valid8 (valid8),
        .data8  (data8),
        .ready4 (ready4),
        .valid4 (valid4),
        .data4  (data4)
    );

    initial begin
        #0.1;
        if (ready8 !== 1'b1) $fatal(1, "ready8: server ready must flow back to the client");
        if (valid8 !== 1'b1) $fatal(1, "valid8: server must see @Valid");
        if (data8 !== 8'hA5) $fatal(1, "data8: payload must pass through Pipe[Word[8]]");
        if (ready4 !== 1'b1) $fatal(1, "ready4: server ready must flow back to the client");
        if (valid4 !== 1'b1) $fatal(1, "valid4: server must see @Valid");
        if (data4 !== 4'h3)  $fatal(1, "data4: payload must pass through Pipe[Word[4]]");

        // The pipes are combinational: new payloads must flow through.
        a8 = 8'h5A;
        a4 = 4'hC;
        #0.1;
        if (data8 !== 8'h5A) $fatal(1, "data8: must follow the new a8");
        if (data4 !== 4'hC)  $fatal(1, "data4: must follow the new a4");

        $display("DONE");
        $finish;
    end
endmodule