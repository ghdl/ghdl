entity test is
end test;

architecture only of test is
  subtype small is integer range 1 to 3;
begin  -- only
p: process
begin  -- process p
 assert small'base'left = integer'left report "TEST FAILED" severity FAILURE;
  report "TEST PASSED" severity NOTE;
  wait;
end process p;
end only;
