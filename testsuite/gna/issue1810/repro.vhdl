entity repro is
end;

architecture behav of repro is
  constant t1: time := 10 ns mod 3 ns;
begin
  assert t1 = 1 ns severity failure;
end;
