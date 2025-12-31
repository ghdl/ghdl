entity name01 is
end;

architecture behav of name01 is
begin
  process
    variable vi : integer;
  begin
    report vi'instance_name;
    wait;
  end process;
end behav;
