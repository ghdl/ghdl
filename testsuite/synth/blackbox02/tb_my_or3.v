module tb_my_or3;
   reg [2:0] a;
   wire res;
   wire       cout;

   my_or3 dut (.a(a[2]), .b(a[1]), .c(a[0]), .x(res));

   initial begin
      a <= 3'b010;
      # 1;
      $display("res=%b", res);
      if (res !== 1'b1)
	$fatal(1, "FAILURE");

      a <= 3'b101;
      # 1;
      $display("res=%b", res);
      if (res !== 1'b1)
	$fatal(1, "FAILURE");

      a <= 3'b000;
      # 1;
      $display("res=%b", res);
      if (res !== 1'b0)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
