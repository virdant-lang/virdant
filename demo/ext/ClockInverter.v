module ClockInverter(
  input  clock,
  input  enable,
  output inv_clock
);
    assign inv_clock = enable & ~clock;
endmodule
