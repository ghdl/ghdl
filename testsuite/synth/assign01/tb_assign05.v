module tb_assign05;
   wire [3:0] b;
   reg [3:0] a;

   assign05 dut (.a(a), .b(b));

   initial begin
      a <= 4'h4;
      # 1;
      if (b !== 4'hb)
	$fatal(1, "FAILURE");

      a <= 4'h5;
      # 1;
      if (b !== 4'ha)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
