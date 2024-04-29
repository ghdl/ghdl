library ieee;
use ieee.math_real.all;

entity tb is
end entity;

architecture test of tb is
begin
  main : process
    constant sine : real := sin(3.14); -- This works
    constant max : real := realmax(1.0, 2.0); -- This leads to 'unhandled call to ieee function "REALMAX"'
    constant tr : real := trunc(3.14); -- This leads to 'unhandled call to ieee function "TRUNC"'
    variable v_max, v_tr : real;
  begin
    wait;
  end process;
end architecture;
