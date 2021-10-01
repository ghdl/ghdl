entity repro4 is
end;

architecture behav of repro4 is
  impure function f(a : bit_vector) return bit_vector
  is
    variable n : natural := 2;
    subtype st is natural range 1 to n;
    constant b : st := 5;
  begin
    assert b /= 0;
    return a;
  end f;
begin
  assert f("01") = "01";
end behav;
