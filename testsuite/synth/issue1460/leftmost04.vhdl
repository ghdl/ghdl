library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity leftmost04 is
  port (d : unsigned (0 downto 1);
        res : out integer);
end leftmost04;

architecture behav of leftmost04 is
begin
  res <= find_leftmost (d, '1');
end behav;

