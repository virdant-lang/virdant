module Tb();
    Top top(
        .clock(clock),
        .reset(reset)
    );

    reg reset = 0;
    reg clock = 0;
    string vcd;

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
        end
    endtask

    initial begin
        if ($value$plusargs("vcd=%s", vcd)) begin
            $display("Saving VCD file: %s", vcd);
            $dumpfile(vcd);
            $dumpvars(0, Tb);
        end
        initialize();
        forever begin
            tick();
        end
    end

endmodule
