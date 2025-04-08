module top (
  input  [2:0] sel,
  input  [7:0] idata,
  output odata
);

  assign odata = idata[sel];

endmodule
