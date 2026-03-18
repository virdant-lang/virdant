module Tb();
    Top top(
        .clock(clock),
        .reset(reset)
    );

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
        end
    endtask

    initial begin
        $dumpvars(0, top);
        initialize();
        forever begin
            tick();
        end
    end

endmodule
