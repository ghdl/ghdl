library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity leftmost01 is
  port (d : unsigned (7 downto 0);
        res : out integer);
end leftmost01;

architecture behav of leftmost01 is
begin
  res <= find_leftmost (d, '1');
end behav;

