entity repro2 is
end repro2;

architecture behav of repro2 is
  signal s : natural;
begin  -- behav
  process (s) is
    variable v : natural;
  begin
    v := s'delayed (10 ns);
  end process;

  process
  begin
    s <= 3;
    wait for 0 ns;
    s <= 4;
    wait for 0 ns;
    s <= 5;
    wait for 0 ns;
    s <= 5;
    wait;
  end process;
end behav;
