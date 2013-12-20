package test_pkg is
  type small_int is range -5 to 5;
end test_pkg;

use work.test_pkg.all;

entity test is
end test;

architecture only of test is
begin  -- only
  p: process
    variable x : small_int;
  begin  -- process p
    x := 3;
    assert ( x = 3 ) report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
    wait;
  end process p;
end only;
