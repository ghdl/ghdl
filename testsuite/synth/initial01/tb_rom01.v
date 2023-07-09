module tb_rom01;
   reg clk;
   reg [2:0] addr;
   wire [3:0] dout;

   rom01 dut (.clk (clk), .addr (addr), .dout(dout));

   initial begin
      addr <= 4'h4;
      clk <= 1'b0;
      #1;
      clk <= 1'b1;
      #1;
      if (dout !== 4'h5)
	$fatal(1, "FAILURE-1");

      addr <= 4'h3;
      clk <= 1'b0;
      #1;
      clk <= 1'b1;
      #1;
      if (dout !== 4'h3)
	$fatal(1, "FAILURE-2");

      $display("PASS");
      $finish;
   end
endmodule
