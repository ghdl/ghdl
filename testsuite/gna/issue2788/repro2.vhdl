package memory is
    Generic(chip_addr : bit_vector(31 downto 0));
end memory;

entity mem is
end mem;

architecture bh of mem is
    package d_memory is new work.memory
        generic map (chip_addr => x"00000000");
begin
end bh;
