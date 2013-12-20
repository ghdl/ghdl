entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'pred(2) = 1  report "TEST FAILED. pred 2 = 1" severity FAILURE;
  report "TEST PASSED pred 2 = 1" severity NOTE;
  assert small'pred(3) = 2  report "TEST FAILED. pred 3 = 2" severity FAILURE;
  report "TEST PASSED pred 3 = 2" severity NOTE;
  wait;
end process p;
end only;
