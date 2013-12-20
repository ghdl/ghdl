entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'rightof(1) = 2  report "TEST FAILED. rightof 1 = 2" severity FAILURE;
  report "TEST PASSED rightof 1 = 2" severity NOTE;
  assert small'rightof(2) = 3  report "TEST FAILED. rightof 2 = 3" severity FAILURE;
  report "TEST PASSED rightof 2 = 3" severity NOTE;
  wait;
end process p;
end only;
