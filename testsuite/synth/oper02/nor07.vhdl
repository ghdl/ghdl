entity nor07 is
  port (a : boolean);
end;

architecture behav of nor07 is
begin
  process
    variable vb : boolean;
    variable vi : integer := 3;
  begin
    assert (a nor (natural(vi) > 2)) = false severity failure;
    wait;
  end process;
end behav;
