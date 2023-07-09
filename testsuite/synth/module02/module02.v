module module02b (input clk,
		  input 	   rst,
		  output reg [7:0] val,
		  output 	   two);
   always @(posedge clk)
      if (rst)
	val <= 0;
      else
	val <= val + 1;

   assign two = val == 2;
endmodule

module module02 (input clk,
		 input 	      rst,
		 output       two);
   module02b dut (.two(two), .rst(rst), .clk(clk));
endmodule
