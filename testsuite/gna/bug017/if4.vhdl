entity if4 is
end;

architecture behav of if4 is
begin
  process
    variable i : natural := 0;
  begin
    report "hello";
    if i = 10 then
        wait for 1 ns;
      else
        report "hello2";
      end if;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
