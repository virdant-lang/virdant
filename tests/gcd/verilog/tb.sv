module Top_tb;

    reg clock;
    reg reset;

    // Instantiate the DUT
    Top dut (
        .clock(clock),
        .reset(reset)
    );

    // Clock: 10ns period
    initial clock = 0;
    always #5 clock = ~clock;

    initial begin
        // Dump waves
        $dumpfile("gcd.vcd");
        $dumpvars(0, Top_tb);

        // Initialize
        reset = 1;
        #5 clock = ~clock;
        #5 clock = ~clock;
        reset = 0;

        // Run for 30 cycles total
        repeat (30) @(posedge clock);

        $finish;
    end

endmodule
