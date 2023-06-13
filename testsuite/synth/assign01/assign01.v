module assign01(output [0:3] a, output [0:3] b, input [0:7] v);
  assign {a, b} = v;
endmodule
