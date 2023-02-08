entity aggr4 is
end;

architecture test of aggr4 is

  signal A1, A0  : bit ;

  function aand (signal v : bit_vector) return bit is
    variable r : bit;
  begin
    r := '1';
    for i in v'range loop
      r := r and v (i);
    end loop;
    return r;
  end aand;
begin
  a1 <= aand (v (1) => A1, v(0) => A0);
end;
