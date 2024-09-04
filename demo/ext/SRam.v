module SRam(
    input         clock,
    input  [13:0] addr,
    input         write_enable,
    input   [3:0] write_mask,
    input  [15:0] data_inp,
    output [15:0] data_out
);
    SB_SPRAM256KA spram
    (
      .CLOCK(clock),
      .ADDRESS(addr),
      .MASKWREN(write_mask),
      .WREN(write_enable),
      .DATAIN(data_inp),
      .DATAOUT(data_out),
      .CHIPSELECT(1'b1),
      .STANDBY(1'b0),
      .SLEEP(1'b0),
      .POWEROFF(1'b1)
    );

endmodule
