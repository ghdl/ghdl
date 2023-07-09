module tb_dff04;

   reg clk;
   reg [7:0] din;
   reg [7:0] dout;

   dff04 dut (clk, din, dout);

   initial begin
      clk <= 0;
      # 1;
      din <= 8'ha7;
      clk <= 1;
      # 1;
      if (dout != 8'ha7)
	$fatal(1, "failure");
      clk <= 0;
      #1;
      din <= 8'hb8;
      clk <= 1;
      # 1;
      if (dout != 8'hb8)
	$fatal(1, "failure");
      $finish;
   end
endmodule
