module tb_dff05;

   reg clk;
   reg rst;
   reg [7:0] din;
   reg [7:0] dout;

   dff05 dut (clk, rst, din, dout);

   initial begin
      clk <= 0;
      rst <= 0;
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

      rst <= 1;
      din <= 8'h23;
      # 1;
      if (dout != 8'h00)
	$fatal(1, "failure");
      clk <= 0;
      rst <= 0;
      #1;
      clk <= 1;
      # 1;
      if (dout != 8'h23)
	$fatal(1, "failure");
      
      $finish;
   end
endmodule
