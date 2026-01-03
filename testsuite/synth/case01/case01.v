module case01 (input [4:0] a, output reg o);
   always @a
     case (a)
       5'b00011:
         o = 1;
       5'b00110, 5'b00111, 5'b10001:
         o = 1;
       5'b00100:
	 o = 0;
       5'b01100:
        o = 1;
       5'b10000:
	 o = 1;
       default:
         o = 0;
     endcase // case (a)
endmodule
