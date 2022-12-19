library ieee;
use ieee.std_logic_1164.all;

entity mwe is
end entity;

architecture tb of mwe is

  constant ic_slv : std_logic_vector(63 downto 0) := x"7000000000000228";
  signal ic_bv : bit_vector(ic_slv'range);

begin

  ic_bv <= to_bitvector(ic_slv);

end architecture;
