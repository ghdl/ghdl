entity call6b is
end;

architecture behav of call6b is
  type my_rec is record
    a, b, c : character;
  end record;
  
  procedure check (s : my_rec) is
  begin
    wait for 1 ns;
    assert s.b = 'a';
  end;
begin
  process
    variable c : character := 'a';
  begin
    check ((a => 'e', b => 'a', c => 'c'));
    report "SUCCESS";
    wait;
  end process;
 
end behav;
