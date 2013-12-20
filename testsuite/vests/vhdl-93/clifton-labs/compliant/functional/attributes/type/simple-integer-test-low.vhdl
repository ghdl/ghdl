entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'low = 1 report "TEST FAILED T low" severity FAILURE;
  report "TEST PASSED T low" severity NOTE;
  wait;
end process p;
end only;
