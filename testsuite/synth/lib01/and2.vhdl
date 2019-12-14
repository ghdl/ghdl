library ieee;
use ieee.std_logic_1164.all;

entity and2 is
  port (a, b : std_logic;
        o : out std_logic);
end and2;

architecture behav of and2 is
begin
  o <= a and b;
end behav;
