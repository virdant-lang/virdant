module Tb;
    reg  master_pull = 0;
    reg  slave_ack   = 0;
    wire seen;

    \top::Top dut(
        .master_pull(master_pull),
        .slave_ack(slave_ack),
        .seen(seen)
    );

    task check(input logic exp, input string msg);
        if (seen !== exp) begin
            $display("FAIL [%0t] %s: seen=%b expected=%b", $time, msg, seen, exp);
            $fatal;
        end
        $display("PASS [%0t] %s", $time, msg);
    endtask

    initial begin
        master_pull = 0; slave_ack = 0;
        #10 check(1'b1, "bus idle: neither participant drives, weak pullup holds SDA high");
        master_pull = 1; slave_ack = 0;
        #10 check(1'b0, "master pulls SDA low: strong low overrides the weak pullup");
        master_pull = 1; slave_ack = 1;
        #10 check(1'b0, "master and slave both pull low: agreeing strong drivers");
        master_pull = 0; slave_ack = 1;
        #10 check(1'b0, "master releases, slave ACK holds SDA low");
        master_pull = 0; slave_ack = 0;
        #10 check(1'b1, "both release: bus returns to idle high");
        $display("=== ALL TESTS PASSED ===");
        $finish;
    end
endmodule
