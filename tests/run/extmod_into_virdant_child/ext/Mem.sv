// Hand-written Verilog implementation of the `ext mod Mem` declared in
// src/mem.vir: a 64 KiB RAM with registered, byte-lane-enabled writes
// and combinational reads.
//
// `vir run` does not execute this file: the simulator models
// `mem::Mem` with a builtin whose behavior matches this module.  Under
// synthesis (or an iverilog run of the emitted Verilog) this is the
// third-party source the `ext mod` binds to.
module \extmod-into-virdant-child::Mem (
    input             clock,
    input  [15:0]     addr,
    input  [31:0]     data_in,
    input             read,
    input             write,
    input  [3:0]      sel,
    output reg [31:0] data_out
);
    reg [31:0] mem [0:16383];

    integer i;
    initial begin
        for (i = 0; i < 16384; i = i + 1) begin
            mem[i] = 32'b0;
        end
    end

    always @(posedge clock) begin
        if (write) begin
            if (sel[0]) mem[addr[15:2]][7:0]   <= data_in[7:0];
            if (sel[1]) mem[addr[15:2]][15:8]  <= data_in[15:8];
            if (sel[2]) mem[addr[15:2]][23:16] <= data_in[23:16];
            if (sel[3]) mem[addr[15:2]][31:24] <= data_in[31:24];
        end
    end

    always @* begin
        data_out = read ? mem[addr[15:2]] : 32'b0;
    end
endmodule
