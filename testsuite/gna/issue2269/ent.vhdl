library ieee;
    use ieee.numeric_bit.all;

entity ent is
end entity;

architecture a of ent is
    constant Bit_c                  : bit                           := '0';
    constant BitVector_c            : bit_vector(3 downto 0)        := (others => Bit_c);
begin
  process begin
    report "String test: " & to_hstring(BitVector_c) severity note;
    wait;
  end process;
end;
