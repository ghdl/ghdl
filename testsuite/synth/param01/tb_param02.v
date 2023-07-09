module tb_param02;
   wire [3:0] res;

   param02 dut (.res(res));

   initial begin
      #1;
      $display("res=%b", res);
      if (res !== 4'hd)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
