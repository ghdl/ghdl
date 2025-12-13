library ieee;
use ieee.numeric_std.all;

entity smax02 is
  port (a : signed(3 downto 0);
        b : integer;
        o : out signed(3 downto 0));
end smax02;

architecture behav of smax02 is
begin
  o <= maximum (a, b);
end behav;
