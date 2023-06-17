module tb_concat01;
   wire [7:0] dout;

   concat01 dut (.o (dout), .h_i(6'b1111_00), .l_i(2'b00));

   initial begin
      #1;
      if (dout !== 8'hf0)
	$fatal(1, "FAILURE-1");
      $display("PASS");
      $finish;
   end
endmodule
