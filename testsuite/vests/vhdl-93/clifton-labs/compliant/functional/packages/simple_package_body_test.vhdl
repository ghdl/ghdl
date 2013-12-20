package test_pkg is
  function return_one
    return integer;
end test_pkg;

package body test_pkg is
  function return_one
    return integer is
  begin  -- return_one
    return 1;
  end return_one;
end test_pkg;


use work.test_pkg.all;

entity test is
end test;

architecture only of test is
begin  -- only
  p: process
  begin  -- process p
    assert ( return_one = 1 ) report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
    wait;
  end process p;
end only;
