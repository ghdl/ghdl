library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity leftmost03 is
  port (d : unsigned (0 to 8);
        res : out integer);
end leftmost03;

architecture behav of leftmost03 is
begin
  res <= find_leftmost (d, '1');
end behav;

