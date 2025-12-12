library ieee;
use ieee.numeric_std.all;

entity err_conv01 is
  port (o : out unsigned(7 downto 0);
        v : in natural;
        w : in natural);
end;

architecture behav of err_conv01 is
begin
  o <= to_unsigned(v, w);
end;
