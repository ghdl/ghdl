entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert (small'ascending)  report "TEST FAILED ascending" severity FAILURE;
  report "TEST PASSED ascending" severity NOTE;
  wait;
end process p;
end only;
