entity test is
end test;

architecture only of test is
  type my_type is array(0 to 3) of integer;
begin  -- only
  p: process
  begin  -- process p
    assert my_type'range'left = 0 report "TEST FAILED" severity failure;
    assert my_type'range'right = 3 report "TEST FAILED" severity failure;
    report "TEST PASSED";
    wait;
  end process p;
end only;
