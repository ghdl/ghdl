library ieee; 
use ieee.std_logic_1164.all;

entity comparator1 is
  port(
    x, y : in std_logic;
    eq : out std_logic
  ); 
end comparator1;

architecture structure of comparator1 is
  signal s0, s1: std_logic; 
begin
  s0 <= (not x) and (not y);
  s1 <= x and y;
  eq <= s0 or s1;
end structure;  
