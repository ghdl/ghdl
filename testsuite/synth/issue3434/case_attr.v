module case_attr (input [1:0] s, input [3:0] a, output reg [3:0] y);
  always @*
    (* full_case *) case (s)
      2'b00: y = a;
      default: y = 4'b0000;
    endcase
endmodule
