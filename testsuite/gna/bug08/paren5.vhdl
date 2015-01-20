entity paren5 is
end paren5;

architecture behav of paren5
is
  procedure a (param : natural) is
  begin
    assert param /= 2;
  end a;
  constant b : natural := 3;
begin
  a(b);
end behav;
