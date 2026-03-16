module Tb();
    Top top(
        .clock(clock)
    );

    reg clock = 0;

    task tick;
        begin
            #5 clock = ~clock;
            #5 clock = ~clock;
        end
    endtask

    initial begin
        forever begin
            tick();
        end
    end

endmodule
