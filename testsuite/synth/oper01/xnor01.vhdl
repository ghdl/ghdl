library ieee;
use ieee.std_logic_1164.all;

entity xnor01 is
  port (l : std_logic_vector(3 downto 0);
        r :  std_logic_vector(3 downto 0);
        o :  out std_logic_vector(3 downto 0));
end xnor01;

architecture behav of xnor01 is
begin
  o <= l xnor r;
end behav;
