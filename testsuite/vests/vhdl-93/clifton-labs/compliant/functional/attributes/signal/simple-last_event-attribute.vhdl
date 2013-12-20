entity test is
end test;

architecture only of test is
  signal s : bit;
begin
  s  <= '1' after 5 ns, '0' after 20 ns, '1' after 30 ns, '0' after 40 ns;
  p: process
   variable v: time;
  begin
   wait for 15 ns;
    v:=s'last_event;
    assert v = 10 ns report "TEST FAILED - s previous value incorrect!" severity failure;
    report "TEST PASSED elapsed time is 10 ns" ;
   wait for 14 ns;
    v:=s'last_event;
    assert v = 9 ns report "TEST FAILED - s previous value incorrect!" severity failure;
    report "TEST PASSED elapsed time is 9 ns" ;
    wait;
  end process;
end architecture only;
