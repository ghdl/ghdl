library ieee;
use ieee.std_logic_1164.all;

entity match07 is
  port (a : in std_logic_vector (3 downto 0);
        z : out std_logic);
end match07;

architecture behav of match07 is
begin
  z <= "1--0" ?/= a;
end behav;
