module module01b (input clk,
		  input 	   rst,
		  output reg [7:0] val,
		  output 	   one);
   always @(posedge clk)
      if (rst)
	val <= 0;
      else
	val <= val + 1;

   assign one = val == 1;
endmodule

module module01 (input clk,
		 input 	      rst,
		 output [7:0] val,
		 output       one);
   module01b dut (.one(one), .val(val), .rst(rst), .clk(clk));
endmodule // module01
