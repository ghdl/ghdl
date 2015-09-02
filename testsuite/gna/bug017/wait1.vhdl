entity wait1 is
end;

architecture behav of wait1  is
begin
  process
  begin
    report "hello";
    wait for 1 ns;
    report "SUCCESS";
    wait;
  end process;
end behav;
