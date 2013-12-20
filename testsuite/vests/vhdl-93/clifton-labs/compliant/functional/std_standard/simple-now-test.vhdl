entity test is
end test;

architecture only of test is
begin  -- only
  p: process
  begin  -- process p
    wait for 1 ns;
    assert now = 1 ns report "TEST FAILED" severity FAILURE;
    wait for 10 ns;
    assert now = 11 ns report "TEST FAILED" severity FAILURE;
    report "PASSED TEST" severity NOTE;
    wait;
  end process p;
end only;
