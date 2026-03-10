library ieee;
use ieee.std_logic_1164.all;

entity my_is_x is
  port (a : std_logic;
        r1 : out boolean;
        r2 : out boolean);
end;

architecture behav of my_is_x is
begin
  r1 <= a = 'X';
  r2 <= is_x(a);
end behav;
