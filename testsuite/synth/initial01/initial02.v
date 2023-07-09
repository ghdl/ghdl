module initial02(input clk, din, en,
		 output reg dout);
   initial
     dout = 0;

   always @(posedge clk)
     if (en)
       dout <= din;
endmodule
