entity test is
end test;

architecture only of test is
  type integer_array is array (0 to 2) of integer;
begin  -- only
  p: process
    variable x : integer_array;
  begin  -- process p
    x := (0, 1, 2);
    assert x(0) = 0 report "TEST FAILED - 0" severity FAILURE;
    assert x(1) = 1 report "TEST FAILED - 1" severity FAILURE;  
    assert x(2) = 2 report "TEST FAILED - 2" severity FAILURE;
    report "TEST PASSED" severity NOTE;
    wait;
  end process p;
end only;
