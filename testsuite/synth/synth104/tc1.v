module tc1(input wire clk,
	   input wire [3:0] sel,
	   output reg 	    a,
	   output reg 	    b);
   always @(posedge clk) begin
      casex (sel)
	2'b10: begin
	   a <= 1;
	   b <= 0;
	end
	2'b0x:
	  a<= 0;
	2'b11:
	  b <= 1;
      endcase // casex (sel)
   end // always @ (posedge clk)
endmodule // tc1
