module tb_assign02;
   wire [7:0] a;
   reg [7:0] v;

   assign02 dut (.r(a), .v(v));

   initial begin
      v <= 8'he4;
      # 1;
      $display("a=%b", a);
      if (a !== 8'he4)
	$fatal(1, "FAILURE");

      v <= 8'h38;
      # 1;
      $display("a=%b", a);
      if (a !== 8'h38)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
