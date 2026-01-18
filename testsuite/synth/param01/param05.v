module param05b (output [3:0] res);
   parameter signed value = 4'h1;

   assign res = value;
endmodule

module param05 (output [3:0] res, input [3:0] add);
   parameter value = -3'sd2;
   wire [3:0] tmp1;

   param05b #(value) sub1 (tmp1);

   assign res = tmp1 + add;
endmodule
