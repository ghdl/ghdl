entity test is
end test;

library ieee;
use ieee.std_logic_1164.all;

architecture only of test is
  signal x, y, result : std_logic := '1';
begin  -- only
  result <= x;
  result <= y;

  process
  begin  -- process
    assert x = '1' report "TEST FAILED" severity failure;
    assert y = '1' report "TEST FAILED" severity failure;
    assert result = '1' report "TEST FAILED" severity failure;

    report "TEST PASSED";
--    x <= 'U';
--    y <= 'U';
    wait;
  end process;
end only;
