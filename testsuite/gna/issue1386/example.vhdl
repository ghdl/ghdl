library ieee;
use ieee.std_logic_1164.all;

entity example is
  generic (
    PARAMETER : std_logic_vector);
end example;

architecture behavioral of example is
begin
  assert PARAMETER = x"9" severity failure;
end behavioral;
