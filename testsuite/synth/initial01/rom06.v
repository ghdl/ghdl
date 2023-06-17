 module fileDemo;

           integer   handle, channels, index, rnd;
           reg [7:0] memory [15:0];

           initial begin
              handle = $fopen("mem.dat");
              channels = handle | 1;
              $display("Generating contents of file mem.dat");
              $fdisplay(channels, "@2");

              for(index = 0; index < 14; index = index + 1) begin
              rnd = $random;
              $fdisplay(channels, "%b", rnd[12:5]);
              end

              $fclose(handle);

              $readmemb("mem.dat", memory);

              $display("\nContents of memory array");
              for(index = 0; index < 16; index = index + 1)
              $displayb(memory[index]);
           end

        endmodule // fileDemo
