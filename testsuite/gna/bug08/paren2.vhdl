entity paren2 is
end paren2;

architecture behav of paren2
is
  subtype b is natural range 1 to 4;
  signal a : bit_vector (b);
begin
  assert a(b) = x"0";
end behav;
