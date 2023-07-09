module tb_rom03;
   reg [2:0] addr;
   wire [3:0] dout;

   rom03 dut (.addr (addr), .dout(dout));

   initial begin
      addr <= 4'h4;
      #1;
      if (dout !== 4'h5)
	$fatal(1, "FAILURE-1");

      addr <= 4'h3;
      #1;
      if (dout !== 4'h3)
	$fatal(1, "FAILURE-2");

      $display("PASS");
      $finish;
   end
endmodule
