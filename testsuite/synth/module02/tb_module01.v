module tb_module01b;
   reg clk;
   reg rst;
   wire [7:0] val;
   wire       one;

   module01 dut (.clk(clk), .rst(rst), .val(val), .one(one));

   initial begin
      rst <= 1;

      clk <= 0;
      #1;
      clk <= 1;
      #1;
      $display("val=%b", val);
      if (val !== 8'h0 || one != 0)
	$fatal(1, "FAILURE");

      rst <= 0;
      clk <= 0;
      # 1;
      clk <= 1;
      #1;
      if (val !== 8'h1 || one != 1)
	$fatal(1, "FAILURE");

      clk <= 0;
      # 1;
      clk <= 1;
      #1;
      if (val !== 8'h2 || one != 0)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
