entity loop1 is
end;

architecture behav of loop1 is
begin
  process
    variable i : natural := 0;
  begin
    report "hello";
    while i < 10 loop
      wait for 1 ns;
      report "hello2";
      i := i + 1;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
