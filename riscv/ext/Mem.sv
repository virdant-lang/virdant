module \cpu::Mem (
    input  wire             clock,
    input  wire     [31:0]  addr,
    input  wire     [31:0]  data_in,
    output reg      [31:0]  data_out
);
    reg [31:0] mem[1024];

    always @(*) begin
        data_out = mem[addr[11:2]];
    end

    always @(posedge clock) begin
        mem[addr[11:2]] <= data_in;
    end
endmodule

module \ifetch::Rom (
    input  wire     [31:0]  addr,
    output reg      [31:0]  instr
);
    reg [31:0] mem[1024];

    always @(*) begin
        instr = mem[addr[11:2]];
    end

    initial begin
        $readmemh("rom.hex", mem);
    end
endmodule
