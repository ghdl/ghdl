module fulladder1(sum, co, a, b, c_in);
   output sum, co;
   input a, b, c_in;
   assign sum = a ^ b ^ c_in;
   assign co = (a & b) | (b & c_in) | (c_in & a);
endmodule

module fulladder4(sum, cout, a, b, cin);
   output [3:0] sum;
   output cout;
   input [3:0] a, b;
   input cin;
   wire [3:1] c;

   // Instantiate four 1-bit full adders
   fulladder1 f0 (sum[0], c[1], a[0], b[0], cin);
   fulladder1 f1 (sum[1], c[2], a[1], b[1], c[1]);
   fulladder1 f2 (sum[2], c[3], a[2], b[2], c[2]);
   fulladder1 f3 (sum[3], cout, a[3], b[3], c[3]);
endmodule
