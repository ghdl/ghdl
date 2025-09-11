library ieee;
use ieee.numeric_std.all;

entity smin01 is
  port (a, b : signed(3 downto 0);
        o : out signed(3 downto 0));
end smin01;

architecture behav of smin01 is
begin
  o <= minimum (a, b);
end behav;
