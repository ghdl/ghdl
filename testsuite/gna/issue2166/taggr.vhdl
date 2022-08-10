entity taggr is
end;

architecture behav of taggr is
  procedure set (v : out string) is
  begin
    v := (others => ' ');
  end set;
begin
  process
    variable s : string (1 to 8);
  begin
    set (s);
    report '<' & s & '>';
    wait;
  end process;
end;
