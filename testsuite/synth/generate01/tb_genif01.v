module tb_genif01;
   wire dout;

   genif01 dut (.o (dout));

   initial begin
      #1;
      if (dout !== 1'b0)
	$fatal(1, "FAILURE-1");
      $display("PASS");
      $finish;
   end
endmodule
