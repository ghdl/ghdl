entity loop2 is
end;

architecture behav of loop2 is
begin
  process
    variable i : natural := 0;
  begin
    report "hello";
    loop
      exit when i = 10;
      wait for 1 ns;
      report "hello2";
      i := i + 1;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
