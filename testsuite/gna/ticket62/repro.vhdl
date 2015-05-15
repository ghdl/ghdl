entity ent is
end;

architecture behav of ent is
  shared variable v : integer;
begin
  process
  begin
    v := 2;
    wait for 2 ns;
    assert v = 5 severity failure;
    wait;
  end process;

  process
  begin
    wait for 1 ns;
    assert v = 2 severity failure;
    v := 5;
    wait;
  end process;
end;
