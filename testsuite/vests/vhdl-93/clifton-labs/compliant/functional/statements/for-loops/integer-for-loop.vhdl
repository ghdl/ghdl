entity test is
end test;

architecture only of test is
begin  -- only
p: process
  variable x : integer;
begin  -- process p
  for i in 1 to 10 loop
    x := i;
  end loop;  -- i
  assert x = 10 report "TEST FAILED x was " & integer'image(x) severity ERROR;
  report "TEST PASSED" severity NOTE;
  wait;
end process p;
end only;
