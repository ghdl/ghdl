entity aggr3 is
end;

architecture test of aggr3 is
  constant tpd         : time := 1 ns ;
  signal A1, A0  : bit ;
  function f return bit_vector is
  begin
    return "01";
  end f;
  procedure asgn (signal o : out bit_vector) is
  begin
    wait for 1 ns;
    o <= f after tpd;
  end asgn;
begin
  process
  begin
    asgn (o (1) => A1, o(0) => A0);
    wait for 1 ns;
    assert a1 = '1';
    assert a0 = '0';
    report "done";
    wait;
  end process;
end;
