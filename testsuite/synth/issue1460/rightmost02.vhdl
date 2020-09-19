library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rightmost02 is
  port (d : signed (2 to 4);
        res : out integer);
end rightmost02;

architecture behav of rightmost02 is
begin
  res <= find_rightmost (d, '1');
end behav;

