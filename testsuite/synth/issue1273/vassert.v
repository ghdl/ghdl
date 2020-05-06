module vassert(input wire clk, input wire [7:0] d, output wire q);
  always @(posedge clk)  begin
    assert(d != 8'ha5);
    q <= ^d;
  end
endmodule
