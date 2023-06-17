module genif03_sub #(logic t)(output logic o);
  if (t)
    assign o = 1'b0;
  else begin
    assign o = 1'b1;
  end
endmodule

module genif03(output o);
  genif03_sub #(1'b0) dut (.o(o));
endmodule
