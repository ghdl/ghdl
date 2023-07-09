module assign03(output reg [0:7] ra,
                output reg [0:7] rb,
                input        clk,
                input [0:7]  a,
                input        wen, 
                input        swap);
   
   always @(posedge clk) begin
      if (swap) begin
         ra <= rb;
         rb <= ra;
      end
      if (wen)
        ra <= a;
      begin
      end
   end
endmodule
