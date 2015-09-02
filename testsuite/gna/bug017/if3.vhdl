entity if3 is
end;

architecture behav of if3 is
begin
  process
    variable i : natural := 0;
  begin
    report "hello";
    loop
      if i = 10 then
        exit;
      else
        report "hello2";
      end if;
      wait for 1 ns;
      i := i + 1;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
