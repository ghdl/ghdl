module tb_toff2;
   wire [7:0] a;

   toff2 dut (a);

   initial begin
      # 1;

      if (a !== 8'ha6)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
