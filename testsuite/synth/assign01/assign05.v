module assign05(input [0:3] a, output [0:3] b);
   wire [0:3] res = ~a;
   assign b = res;
endmodule
