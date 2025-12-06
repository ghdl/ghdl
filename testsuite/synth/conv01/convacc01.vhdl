entity convacc01 is
end;

architecture behav of convacc01 is
  type line is access string;
  subtype line40 is line(1 to 40);
begin
  process
    variable l: line;
    variable l4 : line40;
  begin
    l4 := new string(1 to 40);
    l := l4;
    l4 := l;
    wait;
  end process;
end;
