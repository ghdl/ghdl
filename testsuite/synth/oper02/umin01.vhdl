library ieee;
use ieee.numeric_std.all;

entity umin01 is
  port (a, b : unsigned(3 downto 0);
        o : out unsigned(3 downto 0));
end umin01;

architecture behav of umin01 is
begin
  o <= minimum (a, b);
end behav;
