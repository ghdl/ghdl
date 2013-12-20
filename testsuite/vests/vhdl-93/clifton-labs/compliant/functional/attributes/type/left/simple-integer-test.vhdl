entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'left = 1 report "TEST FAILED" severity FAILURE;
  report "TEST PASSED" severity NOTE;
  wait;
end process p;
end only;
