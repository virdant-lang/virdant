module Testbench();
  reg i = 0;
  initial begin
    $dumpfile("build/out.vcd");
    $dumpvars(0, top);
  end

  reg clock = 1'b0;
  reg miso = 1'b1;
  reg switch0 = 1'b1;

  reg [31:0] counter = 32'b0;

  always #(5) clock = !clock;

  reg reset = 0;

  always @(posedge clock) begin
    if (counter == 1) begin
      reset = 1;
    end else begin
      reset = 0;
    end

//    if (counter % 3 == 0) begin
//      miso = ~miso;
//    end
  end

  Top top(
    .clock(clock),
    .spi__di(miso)
//    .switch0(switch0)
  );

  always @(posedge clock) begin
    counter <= counter + 32'b1;
    if (counter == 32'd100000) begin
        $finish;
    end
  end
endmodule
