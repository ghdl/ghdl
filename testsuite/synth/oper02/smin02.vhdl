library ieee;
use ieee.numeric_std.all;

entity smin02 is
  port (a : signed(3 downto 0);
        b : integer;
        o : out signed(3 downto 0));
end smin02;

architecture behav of smin02 is
begin
  o <= minimum (a, b);
end behav;
