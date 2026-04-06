module Testbench();
    \top::Top top(
        .clock(clock)
    );

    reg clock = 1'b0;
    always #(5) clock = !clock;

    initial begin
        $dumpfile("build/out.vcd");
        $dumpvars(0, top);

        repeat(32) @(posedge clock);

        $finish;
    end
endmodule
