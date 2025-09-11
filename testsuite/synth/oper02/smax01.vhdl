library ieee;
use ieee.numeric_std.all;

entity smax01 is
  port (a, b : signed(3 downto 0);
        o : out signed(3 downto 0));
end smax01;

architecture behav of smax01 is
begin
  o <= maximum (a, b);
end behav;
