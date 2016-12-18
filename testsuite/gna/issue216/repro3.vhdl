entity repro3 is
end repro3;

architecture behav of repro3 is
  constant c : character := 'e';
  constant cond : boolean := c /= 'a';
begin
  assert cond;
end behav;
