entity test_output is
  port (
    output : inout integer := 10
    );
end test_output;

architecture only of test_output is
begin  -- test_output
  test: process
  begin  -- process test
    assert output = 10 report "test failed" severity error;
    assert output /= 10 report "test passed" severity note;
    wait;
  end process test;
end only;
