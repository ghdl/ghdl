entity repro is
end repro;

architecture behav of repro is
  constant a : boolean := True;
  constant b : boolean := False;
  constant c : boolean := False;
begin
  assert (a and b) = c severity failure;
end behav;
