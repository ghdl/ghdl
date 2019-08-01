library ieee;
use ieee.std_logic_1164.all;

entity output01 is
  port (i : std_logic;
        o : out std_logic_vector (1 downto 0));
end output01;

architecture behav of output01 is
begin
  o (0) <= i;
  o (1) <= not i;
end behav;
