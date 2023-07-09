module genif01_sub #(logic t)(output logic o);
  if (t)
    assign o = 1'b0;
  else
    assign o = 1'b1;
endmodule

module genif01(output o);
  genif01_sub #(1'b1) dut (.o(o));
endmodule
