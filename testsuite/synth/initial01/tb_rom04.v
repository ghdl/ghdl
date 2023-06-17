module tb_rom04;
   reg [2:0] addr;
   wire [7:0] dout;

   rom04 dut (.addr (addr), .dout(dout));

   initial begin
      addr <= 4'h2;
      #1;
      if (dout !== 8'hbe)
	$fatal(1, "FAILURE-1");

      addr <= 4'h6;
      #1;
      if (dout !== 8'ha0)
	$fatal(1, "FAILURE-2");

      $display("PASS");
      $finish;
   end
endmodule
