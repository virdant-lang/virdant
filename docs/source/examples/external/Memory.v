module Memory(
  input  wire [15:0] addr,
  output reg  [7:0]  data
);
    reg [7:0] mem[1 << 16];

    always @(*) begin
        data = mem[addr];
    end

    initial begin
        $readmemh("memory.hex", mem);
    end
endmodule
