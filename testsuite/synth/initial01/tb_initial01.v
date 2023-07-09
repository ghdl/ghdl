module tb_initial01;
   reg clk, din, en;
   wire dout;

   initial01 dut (.clk (clk), .din (din), .en (en), .dout(dout));

   initial begin
      clk <= 1'b0;
      din <= 1'b1;
      en <= 1'b1;
      #1;
      if (dout !== 1'b0)
	$fatal(1, "FAILURE-1");

      clk <= 1'b1;
      #1;
      if (dout !== 1'b1)
	$fatal(1, "FAILURE-2");

      $display("PASS");
      $finish;
   end
endmodule
