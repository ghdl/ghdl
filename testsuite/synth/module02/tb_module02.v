module tb_module02;
   reg clk;
   reg rst;
   wire       two;

   module02 dut (.clk(clk), .rst(rst), .two(two));

   initial begin
      rst <= 1;

      clk <= 0;
      #1;
      clk <= 1;
      #1;
      if (two != 0)
	$fatal(1, "FAILURE");

      rst <= 0;
      clk <= 0;
      # 1;
      clk <= 1;
      #1;
      if (two != 0)
	$fatal(1, "FAILURE");

      clk <= 0;
      # 1;
      clk <= 1;
      #1;
      if (two != 1)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
