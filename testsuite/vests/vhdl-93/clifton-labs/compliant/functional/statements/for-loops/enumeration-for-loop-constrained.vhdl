entity test is
end test;

architecture only of test is
begin  -- only
p: process
  type color is ( red, blue, green );
  variable x : color;
begin  -- process p
  for i in red to blue loop
    x := i;
  end loop;  -- i
  assert x = blue report "TEST FAILED x was " & color'image(x) severity ERROR;
  report "TEST PASSED" severity NOTE;
  wait;
end process p;
end only;
