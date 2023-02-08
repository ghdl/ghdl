entity aggr1 is
end;

architecture test of aggr1 is
  constant tpd         : time := 1 ns ;
  signal A1, A0  : bit ;
  function f return bit_vector is
  begin
    return "010";
  end f;
begin
  (A1, A0) <=  f after tpd; -- to_slv(i, 2) after tpd ;
end;
