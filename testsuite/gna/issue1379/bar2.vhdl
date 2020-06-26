entity bar2 is
end bar2;

architecture behav of bar2 is
  function f(v : natural) return natural is
  begin
    return 1;
  end f;
begin
  assert f(v => open) = 1;
end behav;
