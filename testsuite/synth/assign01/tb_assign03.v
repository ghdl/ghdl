module tb_assign03;
   wire [7:0] ra, rb;
   reg        clk;
   reg        wen;
   reg        swap;
   reg [7:0] a;

   assign03 dut (.ra(ra), .rb(rb), .clk(clk), .a(a), .wen(wen), .swap(swap));

   initial begin
      a <= 8'ha5;
      wen <= 1;
      swap <= 0;
      
      clk <= 0;
      #1;
      clk <= 1;
      #1;
      $display("ra=%x, rb=%x", ra, rb);
      if (ra !== 8'ha5)
	$fatal(1, "FAILURE");

      a <= 8'h11;
      swap <= 1;
      clk <= 0;
      #1;
      clk <= 1;
      #1;
      $display("ra=%x, rb=%x", ra, rb);
      if (ra !== 8'h11 || rb !== 8'ha5)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
