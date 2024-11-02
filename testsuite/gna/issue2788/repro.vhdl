package repro_pkg is
    Generic(chip_addr : bit_vector(31 downto 0));
end;

entity repro is
    package d_memory is new work.repro_pkg
        generic map (chip_addr => x"00000000");
end;

architecture bh of repro is
begin
end bh;
