library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rightmost01 is
  port (d : unsigned (7 downto 0);
        res : out integer);
end rightmost01;

architecture behav of rightmost01 is
begin
  res <= find_rightmost (d, '1');
end behav;

