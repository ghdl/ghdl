entity nor03 is
  port (a : boolean);
end;

architecture behav of nor03 is
begin
  process
    variable vb : boolean;
    variable vi : integer := -1;
  begin
    assert a nand (natural(vi) > 0);
    wait;
  end process;
end behav;
