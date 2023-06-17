module rom02(input [3:0] ra,
	   output [15:0] rd);
   reg [15:0] mem[0:15];
   assign rd = mem[ra];

   integer    i, j;
   initial begin
      for (i = 0; i < 16; i = i + 1) begin
         j = i;
         mem[j] = j * j;
      end
   end
endmodule
