library ieee;
use ieee.std_logic_1164.all;

entity slice03 is
  port (di : std_logic_vector(7 downto 0);
        do : out std_logic_vector (3 downto 0));
end slice03;

architecture behav of slice03 is
begin
  do <= di (7 downto 4)(7 downto 4);
end behav;
