library ieee;
use ieee.numeric_std.all;

entity smin03 is
  port (a : integer;
        b : signed(3 downto 0);
        
        o : out signed(3 downto 0));
end smin03;

architecture behav of smin03 is
begin
  o <= minimum (a, b);
end behav;
