module module03b (clk, rst, val);
   input                           clk, rst;
   output [7:0]                    val;
   reg [7:0]                       val;
   parameter freq = 1.0;
   
   always @(posedge clk)
      if (rst)
	val <= 0;
      else
	val <= val + 1;
endmodule

module module03 (input clk,
		 input 	      rst,
		 output       two);
   wire [7:0]                 v;
   module03b #(.freq(5.0)) dut (.val(v), .rst(rst), .clk(clk));

   assign two = v == 2;
endmodule
