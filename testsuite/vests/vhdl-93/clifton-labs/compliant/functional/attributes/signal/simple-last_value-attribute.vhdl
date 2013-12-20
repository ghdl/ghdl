entity test is
end test;

architecture only of test is
  signal s : bit;
begin
  s  <= '1' after 10 ns, '0' after 20 ns, '1' after 30 ns, '0' after 40 ns;
  p: process
   variable v: bit;
  begin
    wait for 1 ns;
   wait for 25 ns;
    v:=s'last_value;
    assert v = '1' report "TEST FAILED - s previous value incorrect!" severity failure;
    report "TEST PASSED v = 1" ;
    wait;
  end process;
end architecture only;
