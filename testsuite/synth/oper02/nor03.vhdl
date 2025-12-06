entity nor03 is
end;

architecture behav of nor03 is
begin
  process
    variable vb : boolean;
    variable vi : integer := -1;
  begin
    assert (natural(vi) > 0) nand vb;
    wait;
  end process;
end behav;
