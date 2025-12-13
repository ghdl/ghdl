library ieee;
use ieee.numeric_std.all;

entity umin03 is
  port (a : natural;
        b : unsigned(3 downto 0);
        o : out unsigned(3 downto 0));
end;

architecture behav of umin03 is
begin
  o <= minimum (a, b);
end behav;
