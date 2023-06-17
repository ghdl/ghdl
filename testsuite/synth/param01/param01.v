module param01b (output [3:0] res);
   parameter value = 4'h1;

   assign res = value;
endmodule


module param01 (output [3:0] res, input [3:0] add);
   parameter value = 4'hb;
   wire [3:0] tmp;

   param01b #(value) sub (tmp);

   assign res = tmp + add;
endmodule
