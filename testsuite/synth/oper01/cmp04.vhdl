library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cmp04 is
  port (l : std_logic_vector(3 downto 0);
        res : out std_logic);
end cmp04;

architecture behav of cmp04 is
begin
  res <= '1' when unsigned(l) < 0 else '0';
end behav;
