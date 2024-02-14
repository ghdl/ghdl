module oa1(input a,
	   input b,
	   input c,
	   output o);

   assign o = (a | b) & c;
endmodule

module oa8(input [7:0] a,
	   input [7:0]	b,
	   input [7:0]	c,
	   output [7:0]	o);

   genvar i;

   for (i = 0; i < 8; i=i+1) begin
      oa1 gate(a[i], b[i], c[i], o[i]);
   end
endmodule
