entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'high = 3 report "TEST FAILED T high" severity FAILURE;
  report "TEST PASSED T high" severity NOTE;
  wait;
end process p;
end only;
