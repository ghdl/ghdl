entity repro4 is
end repro4;

architecture behav of repro4 is
  procedure set (signal v : out string) is
  begin
    v <= (others => ' ');
  end set;
  signal s : string (1 to 3);
begin
  set (s);

  process
  begin
    wait for 0 ns;
    assert s = "   " severity failure;
    wait;
  end process;
end behav;
