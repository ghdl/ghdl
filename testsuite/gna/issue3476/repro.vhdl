library ieee;
use ieee.math_real.all;

entity repro is
end;

architecture behav of repro is
  --  Evaluated during elaboration.
  constant A1 : real := arctan (1.0, 0.0);
  constant A2 : real := arctan (0.0, -1.0);
  constant A3 : real := arctan (-1.0, -1.0);
  constant A4 : real := arctan (1.0);
  constant L1 : real := log (8.0, 2.0);
  constant L2 : real := log (100.0);

  function near (a, b : real) return boolean is
  begin
    --  The results from ieee.math_real are not very good.
    return abs (a - b) < 1.0e-8;
  end near;
begin
  process
  begin
    assert near (A1, MATH_PI_OVER_2) report "arctan (1.0, 0.0)" severity failure;
    assert near (A2, MATH_PI) report "arctan (0.0, -1.0)" severity failure;
    assert near (A3, -3.0 * MATH_PI / 4.0) report "arctan (-1.0, -1.0)" severity failure;
    assert near (A4, MATH_PI / 4.0) report "arctan (1.0)" severity failure;
    assert near (L1, 3.0) report "log (8.0, 2.0)" severity failure;
    assert near (L2, 4.605170185988091) report "log (100.0)" severity failure;
    wait;
  end process;
end behav;
