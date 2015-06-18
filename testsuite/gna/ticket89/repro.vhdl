entity repro is
end repro;

architecture behav of repro is
  signal s : natural;
begin  -- behav
  process (s) is
    variable v : natural;
  begin
    v := s'delayed (0 ns);
  end process;

  process
  begin
    s <= 3;
    wait for 1 ns;
    s <= 4;
    wait for 0 ns;
    s <= 5;
    wait for 0 ns;
    s <= 5;
    wait;
  end process;
end behav;
