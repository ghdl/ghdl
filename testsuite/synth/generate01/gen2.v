module oa8(input [7:0] a,
	   input [7:0]	b,
	   input [7:0]	c,
	   output [7:0]	o);

   genvar i;

   for (i = 0; i < 8; i=i+1) begin
      wire t = a[i] | b[i];
      assign o[i] = t & c[i];
   end
endmodule
