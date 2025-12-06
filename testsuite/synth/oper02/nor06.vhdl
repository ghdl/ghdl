entity nor06 is
end;

architecture behav of nor06 is
begin
  process
    variable vb : boolean;
    variable vi : integer := 5;
  begin
    assert (natural(vi) = 0) nor (natural(vi) = 8) severity failure;
    wait;
  end process;
end behav;
