entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'val(1) = 1 report "TEST FAILED val pos 1" severity FAILURE;
  report "TEST PASSED val pos 1" severity NOTE;
  assert small'val(2) = 2 report "TEST FAILED val pos 2" severity FAILURE;
  report "TEST PASSED val pos 2" severity NOTE;
  assert small'val(3) = 3 report "TEST FAILED val pos 3" severity FAILURE;
  report "TEST PASSED val pos 3" severity NOTE;
  wait;
end process p;
end only;
