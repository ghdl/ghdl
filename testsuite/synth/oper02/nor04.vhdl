entity nor04 is
end;

architecture behav of nor04 is
begin
  process
    variable vb : boolean;
    variable vi : integer := 0;
  begin
    assert (natural(vi) > 0) nand vb severity failure;
    wait;
  end process;
end behav;
