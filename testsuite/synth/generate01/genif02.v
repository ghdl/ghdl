module genif02_sub #(logic t)(output logic o);
  if (t)
    assign o = 1'b0;
  else
    assign o = 1'b1;
endmodule

module genif02(output o);
  genif02_sub #(1'b0) dut (.o(o));
endmodule
