module tb_fulladder2b;
   reg cin;
   reg [1:0] a;
   wire [1:0] res;
   wire       cout;

   fulladder2b dut (.sum(res), .cout(cout), .a(a), .cin(cin));

   initial begin
      a <= 2'h1;
      cin <= 0;
      # 1;
      $display("res=%b", res);
      if (res !== 4'h1 || cout != 0)
	$fatal(1, "FAILURE");

      cin <= 1;
      #1;
      $display("res=%b", res);
      if (res !== 4'h2 || cout != 0)
	$fatal(1, "FAILURE");

      $finish;
   end
endmodule
