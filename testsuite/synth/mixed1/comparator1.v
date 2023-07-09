module comparator1(input x, y,
	           output eq);
   wire s0, s1;

   assign s0 = ~x & ~y;
   assign s1 = x & y;
   assign eq = s0 | s1;
endmodule
