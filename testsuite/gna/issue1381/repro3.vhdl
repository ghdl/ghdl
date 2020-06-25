entity repro3 is
  port (n : out natural);
end;

architecture behav of repro3 is
  impure function f(a : bit_vector) return bit_vector
  is
    subtype st is natural range 1 to n;
  begin
    return a;
  end f;
begin
  assert f("01") = "01";
end behav;
