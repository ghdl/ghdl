module tb_genif03;
   wire dout;

   genif03 dut (.o (dout));

   initial begin
      #1;
      if (dout !== 1'b1)
	$fatal(1, "FAILURE-1");
      $display("PASS");
      $finish;
   end
endmodule
