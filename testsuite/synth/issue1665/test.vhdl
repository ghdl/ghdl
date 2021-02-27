library ieee;
use ieee.fixed_pkg.all;
use ieee.math_real.all;

entity test is
end entity test;

architecture synthesis of test is
  constant test1 : real := (2.0**6); -- works
  constant test2 : real := (2.0**6.0); -- unhandled predefined IEEE operator "**"
  constant test3 : real := arctan(2.0);  -- unhandled call to ieee function "arctan"
  signal a : sfixed(7 downto 0);
begin
a <= to_sfixed(test2, a);
  assert test2 = 64.0 severity failure;
end architecture synthesis;
