entity call6 is
end;

architecture behav of call6 is
  procedure check (s : string) is
  begin
    wait for 1 ns;
    assert s (2) = 'a';
  end;
begin
  process
    variable v : integer := 2;
  begin
    check ("bac");
    wait for 2 ns;
    check ((1 => 'e', 2 => 'a', 3 => 'c'));
    report "SUCCESS";
    wait;
  end process;
 
end behav;
