module Tb();
    Top dut(
        .clock(clock),

        .spi_cs(spi_cs),
        .spi_clk(spi_clk),
        .spi_do(spi_do),
        .spi_di(spi_di)
    );

    reg spi_di = 1;
    always @(posedge spi_clk) begin
        spi_di <= !spi_di;
    end

    reg reset = 0;
    reg clock = 0;

    task tick;
        begin
            #5 clock = ~clock;
            #5 clock = ~clock;
        end
    endtask

    task initialize;
        begin
            reset = 1;
            tick();
            reset = 0;
            tick();
        end
    endtask

    reg i;

    initial begin
        $display("Saving VCD file: %s", "dump.vcd");
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);

        initialize();
        for (i = 0; i < 1000; i++) begin
            tick();
        end
        $finish;
    end

endmodule
