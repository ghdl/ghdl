entity taggr2 is
end;

architecture behav of taggr2 is
  procedure set (v : inout string; l, r : positive) is
  begin
    v (l to r) := (others => ' ');
  end set;
begin
  process
    variable s : string (1 to 8);
  begin
    s(1) := 'A';
    s(8) := 'Z';
    set (s, 2, 7);
    report s;
    wait;
  end process;
end;
