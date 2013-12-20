entity test is
end test;

architecture only of test is
  type my_type is array(0 to 3) of integer;
begin  -- only
  p: process
  begin  -- process p
    assert my_type'right = 3 report "TEST FAILED right = 3" severity failure;
    report "TEST PASSED right = 3";
    wait;
  end process p;
end only;
