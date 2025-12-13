library ieee;
use ieee.numeric_std.all;

entity err_resize01 is
  port (o : out unsigned(7 downto 0);
        v : in unsigned(3 downto 0);
        w : in natural);
end;

architecture behav of err_resize01 is
begin
  o <= resize(v, w);
end;
