library ieee;
use ieee.std_logic_1164.all;

entity match05 is
  port (a : in std_logic_vector (3 downto 0);
        z : out std_logic);
end match05;

architecture behav of match05 is
begin
  z <= "1--0" ?= a;
end behav;
