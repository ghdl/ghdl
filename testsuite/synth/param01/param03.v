module param03b (output [3:0] res);
   parameter signed value = 4'h1;

   assign res = value;
endmodule

module param03 (output [3:0] res, input [3:0] add);
   parameter value = 4'hb;
   wire [3:0] tmp1;

   param03b #(value) sub1 (tmp1);

   assign res = tmp1 + add;
endmodule
