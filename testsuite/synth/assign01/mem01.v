module mem01(output [0:7] r, input [0:7] v,
             input wen, input[0:3] addr, input clk);
   reg [7:0] mem [0:3];

   always @(posedge clk)
     if (wen)
       mem[addr] <= v;
   
   assign r = mem[addr];
endmodule
