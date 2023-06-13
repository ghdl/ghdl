module tb_mem02;
   wire [7:0] r;
   reg [7:0] v;
   reg [3:0] addr;
   reg       clk;

   mem02 dut (.r(r), .v(v), .addr(addr), .clk(clk));

   initial begin
      //  Write 0xe4 at 3
      addr <= 4'h3;
      v <= 8'he4;
      clk <= 0;
      #1;
      
      clk <= 1;
      # 1;

      //  Write 0x83 at 8
      addr <= 4'h8;
      v <= 8'h83;
      clk <= 0;
      #1;

      clk <= 1;
      # 1;

      //  Read at 3 (async)
      addr <= 4'h3;
      clk <= 0;
      #1;

      $display("r=%b", r);
      if (r !== 8'he4)
	$fatal(1, "FAILURE");

      $display("PASS");
      $finish;
   end
endmodule
