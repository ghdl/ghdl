entity call5 is
end;

architecture behav of call5 is
  procedure inc (p : inout integer) is
  begin
    wait for 1 ns;
    p := p + 1;
  end inc;
begin
  process
    variable v : integer := 2;
  begin
    inc (v);
    wait for 2 ns;
    inc (v);
    assert not (v = 4) report "SUCCESS";
    wait;
  end process;
 
end behav;
