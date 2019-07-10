library ieee;
use ieee.std_logic_1164.all;

entity and3 is
  port (a, b, c : std_logic;
        o : out std_logic);
end and3;

architecture behav of and3 is
begin
  o <= a and b and c;
end behav;
