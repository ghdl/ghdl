entity test is
end test;

architecture only of test is
  signal delay_line_in : bit := '0';
  signal delay_line_out : bit := '0';
begin  -- only
  delay: block
  begin  -- block delay
    delay_line_out <= delay_line_in after 1 ns;
  end block delay;

  start: process
  begin  -- process
    delay_line_in <= '1';
    wait;
  end process;

  check: process( delay_line_out )
  begin
    if delay_line_out = '1' then
      assert now = 1 ns report "TEST FAILED - delay did not happen as expected!" severity FAILURE;
      assert not(now = 1 ns) report "TEST PASSED" severity WARNING;
    end if;
  end process;
end only;
