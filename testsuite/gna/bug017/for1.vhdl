entity for1 is
end;

architecture behav of for1 is
begin
  process
  begin
    for i in 1 to 10 loop
      report "hello";
      wait for 1 ns;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
