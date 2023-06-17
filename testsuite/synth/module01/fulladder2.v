module fulladder1(sum, co, a, b, c_in);
   output sum, co;
   input a, b, c_in;

   always @(a, b, c_in)
     $display("fa1: a=%b, b=%b, c_in=%b", a, b, c_in);
   
   assign sum = a ^ b ^ c_in;
   assign co = (a & b) | (b & c_in) | (c_in & a);
endmodule

module fulladder2(sum, cout, a, b, cin);
   output [1:0] sum;
   output cout;
   input [1:0] a, b;
   input cin;
   wire c;
   
   always @(a, b, cin)
     $display("fa2: a=%b, b=%b, cin=%b", a, b, cin);

   // Instantiate four 1-bit full adders
   fulladder1 f0 (sum[0], c, a[0], b[0], cin);
   fulladder1 f2 (sum[1], cout, a[1], b[1], c);
endmodule
