entity test_bench is
end test_bench;

architecture only of test_bench is
  signal sig : integer := 0;
begin  -- only
  p: process
  begin  -- process p
    sig <= 1;
    wait for 1 fs;
    assert sig = 1 report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
    wait;
  end process p;
    
end only;
