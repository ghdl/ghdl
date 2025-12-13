library ieee;
use ieee.numeric_std.all;

entity umin02 is
  port (a : unsigned(3 downto 0);
        b : natural;
        o : out unsigned(3 downto 0));
end;

architecture behav of umin02 is
begin
  o <= minimum (a, b);
end behav;
