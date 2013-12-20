entity test_bench is
end test_bench;

architecture only of test_bench is
  signal sig : integer := 0;
begin  -- only
  assign: process
  begin  -- process p
    sig <= 1;
    wait;
  end process assign;

  check1: process
  begin  -- process check1
    wait for 1 fs;
    assert sig = 1 report "TEST FAILED" severity FAILURE;
    wait;
  end process check1;

  check2: process
  begin  -- process check1
    wait for 1 fs;
    assert sig = 1 report "TEST FAILED" severity FAILURE;
    wait;
  end process check2;

  check3: process
  begin  -- process check1
    wait for 2 fs;
    report "TEST PASSED" severity NOTE;
    wait;
  end process check3;

  
end only;
