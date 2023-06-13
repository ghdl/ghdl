module mem02(output [0:7] r, input [0:7] v,
             input[0:3] addr, input clk);
   reg [7:0]       mem [3:0];

   always @(posedge clk)
     mem[addr] <= v;
   
   assign r = mem[addr];
endmodule
