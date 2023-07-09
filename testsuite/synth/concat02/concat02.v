module concat02(output [7:0] o);
  parameter [1:0] l_i = 2'b00;
  parameter [5:0] h_i = 6'b1010_11;
  assign o = {h_i, l_i};
endmodule
