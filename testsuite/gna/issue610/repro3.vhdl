entity repro3 is
end repro3;

architecture behav of repro3 is
  procedure set (v : out string) is
  begin
    v := (others => ' ');
  end set;
begin
  process
    variable s : string (1 to 4);
  begin
    set (s);
    assert s = "    " severity failure;
    wait;
  end process;
end behav;
