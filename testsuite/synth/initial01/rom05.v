module rom05(input [2:0] addr,
	     output [7:0] dout);

   reg [7:0] rom [7:0];

   assign dout = rom[addr];
endmodule
