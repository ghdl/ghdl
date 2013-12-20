entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'succ(1) = 2  report "TEST FAILED. succ 1 = 2" severity FAILURE;
  report "TEST PASSED succ 1 = 2" severity NOTE;
  assert small'succ(2) = 3  report "TEST FAILED. succ 2 = 3" severity FAILURE;
  report "TEST PASSED succ 2 = 3" severity NOTE;
  wait;
end process p;
end only;
