entity convflt01 is
  generic (s : real := -1.0);
end;

architecture behav of convflt01 is
begin
  process
    variable r2 : real range 0.0 to real'high;
  begin
    r2 := s;
    wait;
  end process;
end;
