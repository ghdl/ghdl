entity repro is
end repro;

architecture behav of repro is
  function exp2 (b : integer) return integer is
  begin
    return 2**b;
  end exp2;
begin
  assert exp2(3) = 8 severity failure;
  assert exp2(31) > 0 severity failure;
end behav;
