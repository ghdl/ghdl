module tb_toff1;
   wire [7:0] a;

   toff1 dut (a);

   initial begin
      # 1;

      if (a !== 8'h24)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
