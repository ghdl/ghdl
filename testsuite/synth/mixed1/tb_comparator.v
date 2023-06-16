module tb_comparator;
   reg a;
   reg b;
   reg res;

   comparator1 dut (.a(a), .b(b), .eq(res));

   initial begin
      a <= 0;
      b <= 0;
      # 1;
      if (res != 1)
        $fatal(1, "bad result");
      $finish;
   end
endmodule // tb_comparator
