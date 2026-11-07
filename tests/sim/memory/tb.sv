module Tb;

    reg clock = 0;
    reg write_enable = 1;
    reg [3:0] addr = 0;
    reg [3:0] read_addr = 0;
    reg [7:0] write_data = 0;
    reg [2:0] rom_addr = 0;
    wire [7:0] ram_read;
    wire [7:0] rom_read;

    \top::Top top (
        .clock(clock),
        .write_enable(write_enable),
        .addr(addr),
        .read_addr(read_addr),
        .write_data(write_data),
        .rom_addr(rom_addr),
        .ram_read(ram_read),
        .rom_read(rom_read)
    );

    always #5 clock = ~clock;

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, Tb);

        // The ROM: $readmemh words, read combinationally.
        rom_addr = 0;
        #1;
        if (rom_read !== 8'h11) $fatal;
        rom_addr = 1;
        #1;
        if (rom_read !== 8'h22) $fatal;
        rom_addr = 2;
        #1;
        if (rom_read !== 8'h33) $fatal;
        rom_addr = 7;
        #1;
        if (rom_read !== 8'h88) $fatal;

        // The RAM: write 0xA5 to address 3, then read it back.
        @(negedge clock);
        addr = 4'd3;
        write_data = 8'hA5;
        write_enable = 1;
        @(negedge clock);
        write_enable = 0;
        read_addr = 4'd3;
        #1;
        if (ram_read !== 8'hA5) $fatal;

        // A second write, read back through the other port.
        @(negedge clock);
        addr = 4'd15;
        write_data = 8'h5A;
        write_enable = 1;
        @(negedge clock);
        write_enable = 0;
        read_addr = 4'd15;
        #1;
        if (ram_read !== 8'h5A) $fatal;

        $display("ALL OK");
        $finish;
    end
endmodule
