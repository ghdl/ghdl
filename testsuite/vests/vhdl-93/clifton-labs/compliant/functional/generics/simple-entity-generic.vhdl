entity test is
  generic ( int_generic : integer := 10);
end test;

architecture only of test is
begin  -- only
  p: process
  begin  -- process p
    assert int_generic = 10 report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
    wait;
  end process p;
end only;
