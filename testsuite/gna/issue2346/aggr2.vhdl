entity aggr2 is
end;

architecture test of aggr2 is
  constant tpd         : time := 1 ns ;
  signal A1, A0  : bit ;
  function f return bit_vector is
  begin
    return "01";
  end f;
  procedure asgn (signal o : out bit_vector) is
  begin
    o <= f after tpd;
  end asgn;
begin
  asgn (o (1) => A1, o(0) => A0);
end;
