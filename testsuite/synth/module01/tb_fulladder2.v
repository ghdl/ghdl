module tb_fulladder2;
   reg cin;
   reg [1:0] a;
   reg [1:0] b;
   wire [1:0] res;
   wire       cout;

   fulladder2 dut (res, cout, a, b, cin);

   initial begin
      a <= 2'h1;
      b <= 2'h2;
      cin <= 0;
      # 1;
      $display("res=%b", res);
      if (res != 4'h3 || cout != 0)
	$fatal(1, "FAILURE");

      cin <= 1;
      #1;
      $display("res=%b", res);
      if (res != 4'h0 || cout != 1)
	$fatal(1, "FAILURE");

      $finish;
   end
endmodule
