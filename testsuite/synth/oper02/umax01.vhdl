library ieee;
use ieee.numeric_std.all;

entity umax01 is
  port (a, b : unsigned(3 downto 0);
        o : out unsigned(3 downto 0));
end umax01;

architecture behav of umax01 is
begin
  o <= maximum (a, b);
end behav;
