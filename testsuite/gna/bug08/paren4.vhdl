entity paren4 is
end paren4;

architecture behav of paren4
is
  function a (param : natural) return natural is
  begin
    return param + 1;
  end a;
  constant b : natural := 3;
begin
  assert a(b) = 4;
end behav;
