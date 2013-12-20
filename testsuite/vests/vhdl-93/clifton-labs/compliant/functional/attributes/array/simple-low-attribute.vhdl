entity test is
end test;

architecture only of test is
  type my_type is array(0 to 3) of integer;
begin  -- only
  p: process
  begin  -- process p
    assert my_type'low = 0 report "TEST FAILED low = 0" severity failure;
    report "TEST PASSED low = 0";
    wait;
  end process p;
end only;
