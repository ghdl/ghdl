entity test is
end test;

architecture only of test is
  type my_type is array(0 to 3) of integer;
begin  -- only
  p: process
  begin  -- process p
    assert my_type'left = 0 report "TEST FAILED left = 0" severity failure;
    report "TEST PASSED left = 0";
    wait;
  end process p;
end only;
