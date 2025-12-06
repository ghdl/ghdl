entity convarr01 is
  generic (s : string := "hello");
end;

architecture behav of convarr01 is
begin
  process
    variable r2 : string(1 to 2);
  begin
    r2 := s;
    wait;
  end process;
end;
