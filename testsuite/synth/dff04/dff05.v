module dff05 (input clk,
              input rst,
	      input [7:0]      din,
	      output reg [7:0] dout);
   always @(posedge clk or posedge rst)
     if (rst)
       dout <= 0;
     else
       dout <= din;
endmodule
