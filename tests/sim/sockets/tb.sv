module Tb;

    \top::Top top (
    );

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);
        #10;

        if (top.\core.mem.addr !== 8) begin
            $fatal;
        end

        if (top.\core.mem.data !== 10) begin
            $fatal;
        end

        if (top.\memory.mem.addr !== 8) begin
            $fatal;
        end

        if (top.\memory.mem.data !== 10) begin
            $fatal;
        end

        if (top.core.\mem.addr !== 8) begin
            $fatal;
        end

        if (top.core.\mem.data !== 10) begin
            $fatal;
        end

        if (top.memory.\mem.addr !== 8) begin
            $fatal;
        end

        if (top.memory.\mem.data !== 10) begin
            $fatal;
        end

        $finish;
    end
endmodule
