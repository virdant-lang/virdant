module Testbench();
  initial begin
    $dumpfile("build/out.vcd");
    $dumpvars(0, top);
  end

  reg clock = 1'b0;
  always #(5) clock = !clock;

  Top top(
    .clock(clock)
  );

  reg [31:0] cycles = 100;

  always @(posedge clock) begin
    cycles <= cycles - 1;
    if (cycles == 0) begin
        $finish;
    end
  end
endmodule
