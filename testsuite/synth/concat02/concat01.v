module concat01(output [7:0] o, input [1:0] l_i, input [5:0] h_i);
  assign o = {h_i, l_i};
endmodule
