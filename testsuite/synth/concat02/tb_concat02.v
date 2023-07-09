module tb_concat02;
   wire [7:0] dout;

   concat02 // #(.h_i(6'b1111_00), .l_i(2'b00))
     dut (.o (dout));

   initial begin
      #1;
      if (dout !== 8'hac)
	$fatal(1, "FAILURE-1");
      $display("PASS");
      $finish;
   end
endmodule
