module tb_rom02;
   reg [3:0] addr;
   wire [15:0] dout;

   rom02 dut (.ra (addr), .rd(dout));

   initial begin
      addr <= 4'h4;
      #1;
      if (dout !== 16'h10)
	$fatal(1, "FAILURE-1");

      addr <= 4'h3;
      #1;
      if (dout !== 16'h9)
	$fatal(1, "FAILURE-2");

      $display("PASS");
      $finish;
   end
endmodule
