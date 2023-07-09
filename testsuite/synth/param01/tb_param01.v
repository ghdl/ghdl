module tb_param01;
   reg [3:0] add;
   wire [3:0] res;

   param01 dut (.res(res), .add(add));

   initial begin
      add <= 4'h3;
      #1;
      $display("res=%b", res);
      if (res !== 4'he)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
