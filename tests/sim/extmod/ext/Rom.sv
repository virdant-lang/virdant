// Synchronous 256-byte ROM.
// Initialized from ext/rom.hex via $readmemh.
module \top::Rom (
    input         clock,
    input  [7:0]  addr,
    output reg [7:0] data
);
    reg [7:0] mem [0:255];

    initial begin
        $readmemh("ext/rom.hex", mem);
    end

    always @(posedge clock) begin
        data <= mem[addr];
    end
endmodule