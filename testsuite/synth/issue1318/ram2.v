`default_nettype none
module memory #(parameter W=8, D=64)
   (input wire clk, wr_en,
    input wire [W-1:0]  wr_addr, rd_addr,
    input wire [D-1:0]  din,
    output reg [D-1:0]  dout);

   (*ram_style = "block"*)
   reg [D-1:0]          mem [(1<<W)-1:0];

   always @(posedge clk) begin
      if(wr_en) mem[wr_addr] <= din;
      dout <= mem[rd_addr];
   end
endmodule // memory
