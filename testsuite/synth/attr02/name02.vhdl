entity name02 is
begin
  process
    variable v : natural;
  begin
    report name02'instance_name;
    wait;
  end process;
end;

architecture behav of name02 is
begin
end behav;
