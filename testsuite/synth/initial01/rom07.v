   module testmemory;
           reg [7:0] memory [9:0];
           integer   index;

           initial begin
              $readmemb("mem.dat", memory);

              for(index = 0; index < 10; index = index + 1)
              $display("memory[%d] = %b", index[4:0], memory[index]);
           end
        endmodule // testmemory
