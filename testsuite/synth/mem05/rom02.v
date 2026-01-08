module rom02 (
    input wire [3:0] addr,
    input  wire [3:0] paddr,	      
    output dat);

    logic [0:14] chars [10];
    initial begin
        chars[0] = 15'b111_101_101_101_111;
        chars[1] = 15'b110_010_010_010_111;
        chars[2] = 15'b111_001_111_100_111;
        chars[3] = 15'b111_001_011_001_111;
        chars[4] = 15'b101_101_111_001_001;
        chars[5] = 15'b111_100_111_001_111;
        chars[6] = 15'b100_100_111_101_111;
        chars[7] = 15'b111_001_001_001_001;
        chars[8] = 15'b111_101_111_101_111;
        chars[9] = 15'b111_101_111_001_001;
    end

   assign dat = chars[addr][paddr];
endmodule
