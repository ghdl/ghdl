entity test is
end test;

architecture only of test is
  -- forward declaration of the function.
  function wired_or( s : bit_vector ) return bit;
  -- declare the subtype.
  subtype rbit is wired_or bit;

  -- declare the actual function.
  function wired_or( s : bit_vector ) return bit is
  begin
      report "resolution function called!" severity note;
    if  ( (s(0) = '1') or (s(1) = '1')) then
      return '1';
    end if;
    return '0';
  end wired_or;

  -- declare a signal of that type.  a resolved signal.
  signal s : rbit;

begin

  -- a concurrent signal assignment.  driver # 1.
  s <= '1';

  testing: process
  begin
    -- verify that resolution function getting called.
    s <= '1' after 10 ns;
    wait on s;
    assert ( s = '1' ) report "TEST FAILED" severity failure;
    report "TEST PASSED";
    wait;
  end process testing;

end only;
