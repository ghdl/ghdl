entity test is
end test;

architecture only of test is
  signal s : bit;
begin
  s  <= '1' after 10 ns, '0' after 20 ns, '1' after 30 ns, '0' after 40 ns;
  p: process
  begin
    wait for 1 ns;
    assert not(s'event) report "TEST FAILED - 'event active" severity failure;
   wait for 25 ns;
 --   s <= '1';
--    wait for 0 ns;
    assert s = '0' report "TEST FAILED - s has not changed  to 0 yet!" severity failure;
    --wait for 10 ns;
    wait for 4 ns;
    assert s = '1' report "TEST FAILED - s has not changed  to 1 yet!" severity failure;
    assert (s'event) report "TEST FAILED - 'event not tripped" severity failure;
    report "TEST PASSED";
    wait;
  end process;
end architecture only;
