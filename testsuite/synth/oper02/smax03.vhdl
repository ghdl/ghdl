library ieee;
use ieee.numeric_std.all;

entity smax03 is
  port (a : integer;
        b : signed(3 downto 0);
        o : out signed(3 downto 0));
end smax03;

architecture behav of smax03 is
begin
  o <= maximum (a, b);
end behav;
