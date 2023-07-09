module rom04(input [2:0] addr,
	     output [7:0] dout);

   //  FIXME: there is warning for:
   //    reg [7:0] rom [7:0];
   //  which is displayed on the standard output.
   reg [7:0] rom [0:7];

   assign dout = rom[addr];

   initial
     $readmemh("rom04.mem", rom);
endmodule
