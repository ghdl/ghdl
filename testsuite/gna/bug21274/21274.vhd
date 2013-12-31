library ieee;
use ieee.std_logic_1164.all;

entity e is
  generic(SIZE: INTEGER := 8);
end entity e;

architecture a of e is
  signal bufreg: STD_LOGIC_VECTOR((2 * SIZE - 1) downto 0);
  alias ADreg1 is bufreg((2 * SIZE - 1) downto SIZE);
  alias ADreg2: std_logic_vector((2 * SIZE - 1) downto SIZE) is bufreg((2 * SIZE - 1) downto SIZE);

begin
end architecture;
