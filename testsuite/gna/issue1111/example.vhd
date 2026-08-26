library ieee;
use ieee.std_logic_1164.all;

entity example is
  generic (
    VEC_WIDTH : positive);
end example;

architecture behavioral of example is
  signal vec : std_logic_vector(VEC_WIDTH-1 downto 0);
begin
end behavioral;
