entity t3 is
end;

architecture behav of t3 is
  constant t1 : time := ps;
begin
  assert time'pos(t1) = 1 severity failure;
end behav;
