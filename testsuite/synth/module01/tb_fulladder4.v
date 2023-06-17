module tb_fulladder4;
   reg cin;
   reg [3:0] a;
   reg [3:0] b;
   wire [3:0] res;
   wire       cout;

   fulladder4 dut (.sum(res), .cout(cout), .a(a), .b(b), .cin(cin));

   initial begin
      a <= 4'h7;
      b <= 4'h6;
      cin <= 0;
      # 1;
      $display("res=%b", res);
      if (res !== 4'hd || cout != 0)
	$fatal(1, "FAILURE");

      cin <= 1;
      #1;
      $display("res=%b", res);
      if (res !== 4'he || cout != 0)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
