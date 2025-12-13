library ieee;
use ieee.numeric_std.all;

entity err_resize02 is
  port (o : out signed(7 downto 0);
        v : in signed(3 downto 0);
        w : in natural);
end;

architecture behav of err_resize02 is
begin
  o <= resize(v, w);
end;
