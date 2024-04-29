library vunit_lib;
context vunit_lib.vunit_context;

library ieee;
use ieee.math_real.all;

entity tb is
  generic (runner_cfg : string);
end entity;

architecture test of tb is
begin
  main : process
    constant sine : real := sin(3.14); -- This works
--    constant max : real := realmax(1.0, 2.0); -- This leads to 'unhandled call to ieee function "REALMAX"'
--    constant tr : real := trunc(3.14); -- This leads to 'unhandled call to ieee function "TRUNC"'
    variable v_max, v_tr : real;
  begin
    test_runner_setup(runner, runner_cfg);

    v_max := realmax(1.0, 2.0); -- This works
    report to_string(v_max);

    v_tr := trunc(3.14); -- This works
    report to_string(v_tr);

    test_runner_cleanup(runner);
  end process;
end architecture;
