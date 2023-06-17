module fulladder1b(sum, co, a, c_in);
   output sum, co;
   input a, c_in;

//   always @(a, c_in)
//     $display("fa1: a=%b, c_in=%b", a, c_in);

   assign sum = a ^ c_in;
   assign co = c_in & a;
endmodule

module fulladder2b(sum, cout, a, cin);
   output [1:0] sum;
   output cout;
   input [1:0] a;
   input cin;
   wire c;

//   always @(a, cin)
//     $display("fa2: a=%b, cin=%b", a, cin);

   fulladder1b f0 (sum[0], c, a[0], cin);
   fulladder1b f1 (sum[1], cout, a[1], c);
endmodule
