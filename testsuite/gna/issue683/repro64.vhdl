entity repro64 is
end repro64;

architecture behav of repro64 is
  type integer64 is range -2**62 to 2**62 - 2;
  
  function exp2 (b : integer) return integer64 is
  begin
    return 2**b;
  end exp2;
begin
  assert exp2(3) = 8 severity failure;
  assert exp2(31) = 2**31 severity failure;
end behav;
