module assign02(output [0:7] r, input [0:7] v);
   reg [7:0] t;

   always @(*)
     {t[7:4], t[3:0]} <= v;

   assign r = t;
endmodule
