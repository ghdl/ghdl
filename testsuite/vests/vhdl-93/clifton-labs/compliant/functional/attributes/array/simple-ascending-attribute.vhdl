entity test is
end test;

architecture only of test is
  type my_type is array(0 to 3) of integer;
begin  -- only
  p: process
  begin  -- process p
    assert (my_type'ascending) report "TEST FAILED ascending" severity failure;
    report "TEST PASSED ascending";
    wait;
  end process p;
end only;
