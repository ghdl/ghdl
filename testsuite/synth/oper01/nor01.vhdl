library ieee;
use ieee.std_logic_1164.all;

entity nor01 is
  port (l : std_logic_vector(3 downto 0);
        r :  std_logic_vector(3 downto 0);
        o :  out std_logic_vector(3 downto 0));
end nor01;

architecture behav of nor01 is
begin
  o <= l nor r;
end behav;
