module tb_dff01;
   reg clk;
   reg en;
   reg [7:0] din;
   reg [7:0] dout;

   dff01 dut (clk, en, din, dout);

   initial begin
      clk <= 0;
      en <= 1;
      # 1;
      din <= 8'ha7;
      clk <= 1;
      # 1;
      if (dout != 8'ha7)
	$fatal(1, "failure");
      clk <= 0;
      en <= 0;
      #1;
      din <= 8'hb8;
      clk <= 1;
      # 1;
      if (dout != 8'ha7)
	$fatal(1, "failure");
      $finish;
   end
endmodule
