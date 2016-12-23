entity call6a is
end;

architecture behav of call6a is
  procedure check (s : string) is
  begin
    wait for 1 ns;
    assert s (2) = 'a';
  end;
begin
  process
    variable c : character := 'a';
  begin
    check ("bac");
    wait for 2 ns;
    check ((1 => 'e', 2 => c, 3 => 'c'));
    report "SUCCESS";
    wait;
  end process;
 
end behav;
