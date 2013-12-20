entity test is
end test;

architecture only of test is
  type small is range 1 to 3;
begin  -- only
p: process
begin  -- process p
  assert small'image(1) = "1" report "TEST FAILED image 1" severity FAILURE;
  report "TEST PASSED image 1" severity NOTE;
  assert small'image(2) = "2" report "TEST FAILED image 2" severity FAILURE;
  report "TEST PASSED image 2" severity NOTE;
  assert small'image(3) = "3" report "TEST FAILED image 3" severity FAILURE;
  report "TEST PASSED image 3" severity NOTE;
  wait;
end process p;
end only;
