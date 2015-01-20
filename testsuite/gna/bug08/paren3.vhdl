entity paren3 is
end paren3;

architecture behav of paren3
is
  subtype a is integer;
  constant b : real := 3.15;
begin
  assert a(b) = 3;
end behav;
