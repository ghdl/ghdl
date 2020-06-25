entity repro2 is
  port (o : out bit);
end;

architecture behav of repro2 is
  function idf (a : bit) return bit is
  begin
    return a;
  end idf;

  type id_arr is array(bit) of bit;

  constant idc : id_arr := ('0' => '0', '1' => '1');

  function f(a : bit_vector) return bit_vector
  is
    variable v : bit_vector(1 to a'length) := a;
    variable r : bit_vector(1 to a'length);
  begin
    for i in v'range loop
      r(i) := idc(v (i));
    end loop;
    return r;
  end f;
begin
  assert f("01") = "01";
  assert idc(o) = '1';
end behav;
