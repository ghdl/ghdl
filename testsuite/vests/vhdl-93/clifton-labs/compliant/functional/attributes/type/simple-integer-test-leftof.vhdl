entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'leftof(2) = 1  report "TEST FAILED. leftof 2 = 1" severity FAILURE;
  report "TEST PASSED leftof 2 = 1" severity NOTE;
  assert small'leftof(3) = 2  report "TEST FAILED. leftof 3 = 2" severity FAILURE;
  report "TEST PASSED leftof 3 = 2" severity NOTE;
  wait;
end process p;
end only;
