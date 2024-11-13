entity mem_tb2 is
end;

architecture bh of mem_tb2 is
    type mem_block_t is protected
        procedure test;
    end protected;
    type mem_block_t is protected body

        procedure readHexByte(value : out bit_vector(7 downto 0)) is
        begin
        end procedure;

        procedure test is
            variable value : bit_vector(7 downto 0);
        begin
            readHexByte(value);
        end procedure;
    end protected body;

    shared variable m : mem_block_t;
begin
end bh;
