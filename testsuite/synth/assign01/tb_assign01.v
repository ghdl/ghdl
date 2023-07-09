module tb_assign01;
   wire [3:0] a;
   wire [3:0] b;
   reg [7:0] v;

   assign01 dut (.a(a), .b(b), .v(v));

   initial begin
      v <= 8'he4;
      # 1;
      $display("a=%b, b=%b", a, b);
      if (a !== 4'he || b !== 4'h4)
	$fatal(1, "FAILURE");

      v <= 8'h38;
      # 1;
      $display("a=%b, b=%b", a, b);
      if (a !== 4'h3 || b !== 4'h8)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
