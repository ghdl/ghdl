module my_or3 (input a, b, c, output x);
   wire t;
//   my_or ior1 (a, b, t), ior2(c, t, x);
   my_or ior1 (a, b, t);

   my_or ior2(c, t, x);
endmodule
