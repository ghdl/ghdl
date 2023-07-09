module rom01(input clk, [2:0] addr,
	     output reg [3:0] dout);

   reg [3:0] rom [7:0];

   always @(posedge clk)
     dout <= rom[addr];

   initial begin
      rom [0] = 4'h1;
      rom [1] = 4'h2;
      rom [2] = 4'h4;
      rom [3] = 4'h3;
      rom [4] = 4'h5;
      rom [5] = 4'h7;
      rom [6] = 4'h0;
      rom [7] = 4'h6;
   end
endmodule
