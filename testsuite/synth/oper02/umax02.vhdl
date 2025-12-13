library ieee;
use ieee.numeric_std.all;

entity umax02 is
  port (a : unsigned(3 downto 0);
        b : natural;
        o : out unsigned(3 downto 0));
end umax02;

architecture behav of umax02 is
begin
  o <= maximum (a, b);
end behav;
