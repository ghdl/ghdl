library ieee;
use ieee.numeric_std.all;

entity umax03 is
  port (a : natural;
        b : unsigned(3 downto 0);
        o : out unsigned(3 downto 0));
end umax03;

architecture behav of umax03 is
begin
  o <= maximum (a, b);
end behav;
