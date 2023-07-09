module initial01(input clk, din, en,
		 output reg dout);
   always @(posedge clk)
     if (en)
       dout <= din;

   initial
     dout = 0;
endmodule
