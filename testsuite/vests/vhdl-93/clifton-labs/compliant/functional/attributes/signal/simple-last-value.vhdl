entity test is
end test;

architecture only of test is
  signal s : bit := '0';
begin
  p : process
  begin
    s <= '1';
    wait for 0 fs;
    assert s'last_value = '0' report "TEST FAILED" severity failure;
    report "TEST PASSED";
    wait;
  end process;
end only;
