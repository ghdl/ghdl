(* RAM_STYLE="BLOCK" *)
reg [7:0] lineMem [0:31];
    
reg [15:0]     column_data = 0;
reg   [ADDR_BITS - 1:0]   line_mem_read_address  = 0;

always @(posedge clk) begin 
       column_data[7:0]     <= lineMem[line_mem_read_address];
       column_data[15:8]   <= data_in;
end
