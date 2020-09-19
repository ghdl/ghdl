library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity leftmost02 is
  port (d : signed (8 to 12);
        res : out integer);
end leftmost02;

architecture behav of leftmost02 is
begin
  res <= find_leftmost (d, '1');
end behav;

