entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'value("1") = 1 report "TEST FAILED value 1" severity FAILURE;
  report "TEST PASSED value 1" severity NOTE;
  assert small'value("2") = 2 report "TEST FAILED value 2" severity FAILURE;
  report "TEST PASSED value 2" severity NOTE;
  assert small'value("3") = 3 report "TEST FAILED value 3" severity FAILURE;
  report "TEST PASSED value 3" severity NOTE;
  wait;
end process p;
end only;
